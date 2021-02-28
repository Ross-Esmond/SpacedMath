(ns spacedmath.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [spacedmath.core-test]))

(doo-tests 'spacedmath.core-test)

