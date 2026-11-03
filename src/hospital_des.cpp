// Compiled bed-flow engine for the capacity search and the dashboard runs
// (see R/core/fast_engine.R).
//
// Reproduces the patient-flow rules of the simmer model in
// R/core/hospital_trajectory.R (run_pathway_step() and new_bed_dispatcher())
// without simmer's per-event R callbacks or monitoring tables:
//  * Every bed request joins one shared dispatcher. Requests from patients who
//    already hold a bed (boarding) are served before fresh requests; within a
//    tier, in registration order. Each request takes the first candidate unit
//    (primary, then fallbacks in order) with a free bed, and a request that
//    cannot be served does not block later ones.
//  * A boarder releases the bed it holds when it is admitted, and that freed
//    bed is offered again immediately.
//  * Boarding credit (credit = true, never for a bed held in an ED unit): the
//    wait counts toward the step's stay; when the wait reaches the whole stay,
//    the request is dropped and the patient moves on keeping the held bed.
//  * Capacity switches from the warm-up values to the event values at
//    switch_time (negative = never), followed by a dispatch.
// Arrival times and stays are drawn in R with the same random-number streams as
// the simmer model, so both engines see identical patients.
//
// Returned metrics follow get_hospital_mon_resources() and
// waiting_state_summary() over the observation window [obs_start, obs_end]:
// peak occupied beds (state at obs_start included), person-days of fresh
// (bed-less) waiting, and fresh requests started in [obs_start, obs_end), all
// by primary unit.
//
// With record = true it also returns the logs the dashboard tables are built
// from: every change of a unit's occupied beds or fresh-waiting count, every
// visit (a bed held, a fresh wait or a boarding wait) and each patient's
// discharge time. Same-instant events may be logged in a different order than
// simmer's monitor, which only affects figures that average recorded states.

#include <Rcpp.h>
#include <algorithm>
#include <deque>
#include <list>
#include <queue>
#include <utility>
#include <vector>

namespace {

enum EventType { CAPACITY_SWITCH = 0, ARRIVAL = 1, STAY_END = 2, STAY_SERVED_WHILE_WAITING = 3 };

struct Event {
  double time;
  long long seq;  // insertion order breaks ties, as simmer does for equal times
  int type;
  int patient;
  long long token;
};

struct LaterFirst {
  bool operator()(const Event& a, const Event& b) const {
    if (a.time != b.time) return a.time > b.time;
    return a.seq > b.seq;
  }
};

struct Patient {
  int first_step = 0;   // offset of this patient's pathway in step_unit / step_los
  int n_steps = 0;
  int step = -1;        // current pathway step, 0-based
  int bed = -1;         // unit whose bed the patient holds, -1 = none
  bool pending = false;
  bool boarding = false;
  bool credit = false;
  double request_time = 0;
  long long credit_token = 0;  // 0 = no credited wait in progress
  std::list<int>::iterator where;
  int bed_visit = -1;          // open visit logs (record mode only)
  int request_visit = -1;
};

enum VisitKind { BED = 0, FRESH_WAIT = 1, BOARDING_WAIT = 2 };

class BedFlow {
 public:
  BedFlow(const std::vector<double>& warmup_capacity, const std::vector<double>& event_capacity,
          double switch_time, const std::vector<std::vector<int>>& candidates,
          const std::vector<bool>& is_ed, const std::vector<double>& arrival,
          const std::vector<int>& n_steps, const std::vector<int>& step_unit,
          const std::vector<double>& step_los, bool credit_on, double obs_start, double obs_end,
          bool record)
      : capacity_(warmup_capacity), event_capacity_(event_capacity), candidates_(candidates),
        is_ed_(is_ed), step_unit_(step_unit), step_los_(step_los), credit_on_(credit_on),
        obs_start_(obs_start), obs_end_(obs_end), record_(record) {
    const int n_units = static_cast<int>(capacity_.size());
    occupied_.assign(n_units, 0.0);
    waiting_.assign(n_units, 0.0);
    max_occupied_.assign(n_units, 0.0);
    person_days_.assign(n_units, 0.0);
    episodes_.assign(n_units, 0.0);
    patients_.resize(arrival.size());
    if (record_) discharge_time_.assign(arrival.size(), NA_REAL);
    int offset = 0;
    for (std::size_t i = 0; i < arrival.size(); ++i) {
      patients_[i].first_step = offset;
      patients_[i].n_steps = n_steps[i];
      offset += n_steps[i];
    }
    if (switch_time >= 0) schedule(switch_time, CAPACITY_SWITCH, -1, 0);
    for (std::size_t i = 0; i < arrival.size(); ++i) {
      schedule(arrival[i], ARRIVAL, static_cast<int>(i), 0);
    }
  }

  void run() {
    while (!events_.empty()) {
      const Event event = events_.top();
      if (event.time >= obs_end_) break;
      events_.pop();
      advance_to(event.time);
      switch (event.type) {
        case CAPACITY_SWITCH:
          for (std::size_t u = 0; u < capacity_.size(); ++u) {
            const bool changed = capacity_[u] != event_capacity_[u];
            capacity_[u] = event_capacity_[u];
            if (changed) log_state(static_cast<int>(u));
          }
          dispatch();
          break;
        case ARRIVAL:
          patients_[event.patient].step = 0;
          request(event.patient);
          break;
        case STAY_END:
          next_step(event.patient);
          break;
        case STAY_SERVED_WHILE_WAITING:
          served_while_waiting(event.patient, event.token);
          break;
      }
    }
    advance_to(obs_end_);
    record_start_state();
  }

  Rcpp::List metrics() const {
    Rcpp::List result = Rcpp::List::create(Rcpp::Named("max_occupied") = max_occupied_,
                                           Rcpp::Named("wait_person_days") = person_days_,
                                           Rcpp::Named("wait_episodes") = episodes_);
    if (!record_) return result;
    // Visits still open at the end of the run keep end = NA.
    result["states"] = Rcpp::List::create(
        Rcpp::Named("time") = state_time_, Rcpp::Named("unit") = state_unit_,
        Rcpp::Named("server") = state_server_, Rcpp::Named("queue") = state_queue_,
        Rcpp::Named("capacity") = state_capacity_);
    result["visits"] = Rcpp::List::create(
        Rcpp::Named("patient") = visit_patient_, Rcpp::Named("kind") = visit_kind_,
        Rcpp::Named("unit") = visit_unit_, Rcpp::Named("start") = visit_start_,
        Rcpp::Named("end") = visit_end_);
    result["discharge_time"] = discharge_time_;
    return result;
  }

 private:
  std::vector<double> capacity_, event_capacity_;
  std::vector<std::vector<int>> candidates_;
  std::vector<bool> is_ed_;
  std::vector<int> step_unit_;
  std::vector<double> step_los_;
  bool credit_on_;
  double obs_start_, obs_end_;
  bool record_;

  std::vector<Patient> patients_;
  std::priority_queue<Event, std::vector<Event>, LaterFirst> events_;
  long long next_seq_ = 0;
  long long next_token_ = 0;
  double now_ = 0;
  double last_time_ = 0;
  bool start_recorded_ = false;

  std::vector<double> occupied_;  // includes beds just assigned (the dispatcher's reservations)
  std::vector<double> waiting_;   // fresh (bed-less) requests, by primary unit
  std::list<int> boarding_requests_, fresh_requests_;
  std::deque<std::pair<int, int>> admissions_;
  bool admitting_ = false;

  std::vector<double> max_occupied_, person_days_, episodes_;

  // Logs (record mode only).
  std::vector<double> state_time_, state_server_, state_queue_, state_capacity_;
  std::vector<int> state_unit_;
  std::vector<int> visit_patient_, visit_kind_, visit_unit_;
  std::vector<double> visit_start_, visit_end_;
  std::vector<double> discharge_time_;

  void schedule(double time, int type, int patient, long long token) {
    events_.push(Event{time, next_seq_++, type, patient, token});
  }

  void log_state(int unit) {
    if (!record_) return;
    state_time_.push_back(now_);
    state_unit_.push_back(unit);
    state_server_.push_back(occupied_[unit]);
    state_queue_.push_back(waiting_[unit]);
    state_capacity_.push_back(capacity_[unit]);
  }

  int open_visit(int id, int kind, int unit) {
    if (!record_) return -1;
    visit_patient_.push_back(id);
    visit_kind_.push_back(kind);
    visit_unit_.push_back(unit);
    visit_start_.push_back(now_);
    visit_end_.push_back(NA_REAL);
    return static_cast<int>(visit_end_.size()) - 1;
  }

  void close_visit(int& visit) {
    if (visit >= 0) visit_end_[visit] = now_;
    visit = -1;
  }

  // The state at obs_start is the one left by every event up to obs_start.
  void record_start_state() {
    if (start_recorded_) return;
    start_recorded_ = true;
    for (std::size_t u = 0; u < occupied_.size(); ++u) {
      max_occupied_[u] = std::max(max_occupied_[u], occupied_[u]);
    }
  }

  void advance_to(double time) {
    if (time > obs_start_) record_start_state();
    const double lower = std::max(last_time_, obs_start_);
    const double upper = std::min(time, obs_end_);
    if (upper > lower) {
      for (std::size_t u = 0; u < waiting_.size(); ++u) person_days_[u] += waiting_[u] * (upper - lower);
    }
    last_time_ = std::max(last_time_, time);
    now_ = time;
  }

  void occupancy_changed(int unit) {
    if (now_ > obs_start_ && now_ <= obs_end_) {
      max_occupied_[unit] = std::max(max_occupied_[unit], occupied_[unit]);
    }
    log_state(unit);
  }

  int primary_unit(const Patient& patient) const {
    return step_unit_[patient.first_step + patient.step];
  }

  void request(int id) {
    Patient& patient = patients_[id];
    const int primary = primary_unit(patient);
    patient.pending = true;
    patient.boarding = patient.bed >= 0;
    patient.credit = patient.boarding && credit_on_ && !is_ed_[patient.bed];
    patient.request_time = now_;
    if (!patient.boarding) {
      waiting_[primary] += 1;
      if (now_ >= obs_start_ && now_ < obs_end_) episodes_[primary] += 1;
      log_state(primary);
    }
    patient.request_visit = open_visit(id, patient.boarding ? BOARDING_WAIT : FRESH_WAIT, primary);
    std::list<int>& tier = patient.boarding ? boarding_requests_ : fresh_requests_;
    patient.where = tier.insert(tier.end(), id);
    if (patient.credit) {
      patient.credit_token = ++next_token_;
      schedule(now_ + step_los_[patient.first_step + patient.step], STAY_SERVED_WHILE_WAITING, id,
               patient.credit_token);
    }
    dispatch();
  }

  // One pass of the dispatcher; assigned beds are counted as occupied at once
  // and the admissions themselves are completed by dispatch().
  void assign_free_beds() {
    std::vector<double> free_beds(capacity_.size());
    bool any_free = false;
    for (std::size_t u = 0; u < capacity_.size(); ++u) {
      free_beds[u] = capacity_[u] - occupied_[u];
      if (free_beds[u] >= 1) any_free = true;
    }
    if (!any_free) return;
    for (std::list<int>* tier : {&boarding_requests_, &fresh_requests_}) {
      for (auto it = tier->begin(); it != tier->end();) {
        Patient& patient = patients_[*it];
        int chosen = -1;
        for (int unit : candidates_[primary_unit(patient)]) {
          if (free_beds[unit] >= 1) {
            chosen = unit;
            break;
          }
        }
        if (chosen < 0) {
          ++it;
          continue;
        }
        free_beds[chosen] -= 1;
        occupied_[chosen] += 1;
        occupancy_changed(chosen);
        patient.pending = false;
        admissions_.emplace_back(*it, chosen);
        it = tier->erase(it);
      }
    }
  }

  void dispatch() {
    assign_free_beds();
    if (admitting_) return;
    admitting_ = true;
    while (!admissions_.empty()) {
      const std::pair<int, int> admission = admissions_.front();
      admissions_.pop_front();
      admit(admission.first, admission.second);
    }
    admitting_ = false;
  }

  void admit(int id, int unit) {
    Patient& patient = patients_[id];
    const double los = step_los_[patient.first_step + patient.step];
    close_visit(patient.request_visit);
    if (!patient.boarding) {
      waiting_[primary_unit(patient)] -= 1;
      log_state(primary_unit(patient));
    }
    const double stay = patient.credit ? std::max(0.0, los - (now_ - patient.request_time)) : los;
    patient.credit_token = 0;
    if (patient.boarding) {
      close_visit(patient.bed_visit);
      occupied_[patient.bed] -= 1;
      occupancy_changed(patient.bed);
      assign_free_beds();
    }
    patient.bed = unit;
    patient.bed_visit = open_visit(id, BED, unit);
    schedule(now_ + stay, STAY_END, id, 0);
  }

  void release_bed(int id) {
    Patient& patient = patients_[id];
    close_visit(patient.bed_visit);
    if (record_) discharge_time_[id] = now_;
    occupied_[patient.bed] -= 1;
    occupancy_changed(patient.bed);
    patient.bed = -1;
    dispatch();
  }

  void next_step(int id) {
    Patient& patient = patients_[id];
    patient.step += 1;
    if (patient.step >= patient.n_steps) {
      release_bed(id);
    } else {
      request(id);
    }
  }

  // Credited boarding: the step was completed while waiting for its bed.
  void served_while_waiting(int id, long long token) {
    Patient& patient = patients_[id];
    if (!patient.pending || patient.credit_token != token) return;
    boarding_requests_.erase(patient.where);
    close_visit(patient.request_visit);
    patient.pending = false;
    patient.credit_token = 0;
    dispatch();
    next_step(id);
  }
};

}  // namespace

// [[Rcpp::export]]
Rcpp::List hospital_des_run(Rcpp::NumericVector warmup_capacity, Rcpp::NumericVector event_capacity,
                            double switch_time, Rcpp::List candidates, Rcpp::LogicalVector is_ed,
                            Rcpp::NumericVector arrival, Rcpp::IntegerVector n_steps,
                            Rcpp::IntegerVector step_unit, Rcpp::NumericVector step_los,
                            bool credit, double obs_start, double obs_end, bool record = false) {
  std::vector<std::vector<int>> candidate_units;
  for (R_xlen_t u = 0; u < candidates.size(); ++u) {
    candidate_units.push_back(Rcpp::as<std::vector<int>>(candidates[u]));
  }
  BedFlow flow(Rcpp::as<std::vector<double>>(warmup_capacity),
               Rcpp::as<std::vector<double>>(event_capacity), switch_time, candidate_units,
               Rcpp::as<std::vector<bool>>(is_ed), Rcpp::as<std::vector<double>>(arrival),
               Rcpp::as<std::vector<int>>(n_steps), Rcpp::as<std::vector<int>>(step_unit),
               Rcpp::as<std::vector<double>>(step_los), credit, obs_start, obs_end, record);
  flow.run();
  return flow.metrics();
}
