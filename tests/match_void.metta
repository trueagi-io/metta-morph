!(bind! &belief_events (new-space))
!(bind! &goal_events (new-space))
!(add-atom &belief_events belief1)
!(case (match &belief_events $1 $1)
       ((%void% 42)))
!(case (match &belief_events $1 $1)
       ((belief1 yes) (%void% 42)))
!(case (match &goal_events $1 $1)
       ((%void% 42)))
!(case (match &goal_events $1 $1)
       ((goal1 yes) (%void% 42)))
!(add-atom &goal_events goal1)
!(case (match &goal_events $1 $1)
       ((goal1 yes) (%void% 42)))
!(case (match &goal_events $1 $1)
       ((%void% 42)))
