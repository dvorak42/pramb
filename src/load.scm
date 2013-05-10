(load "utils" user-initial-environment)
(load "generics" user-initial-environment)
(load "syntax" user-initial-environment)
(load "proc" user-initial-environment)
(load "env" user-initial-environment)

(define generic-evaluation-environment
  (extend-top-level-environment user-initial-environment))

(load "analyze" generic-evaluation-environment)
(load "ambc" generic-evaluation-environment)
(load "repl" generic-evaluation-environment)

(ge generic-evaluation-environment)
