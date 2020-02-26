(require 'shackle)
(shackle-mode 1)

(add-to-list 'shackle-rules '("\\*helm" :regexp t :align t :size 0.5 :popup t))
(add-to-list 'shackle-rules '("\\*anything" :regexp t :align t :size 0.5 :popup t))
(add-to-list 'shackle-rules '("*quickrun*" :align t :size 0.5 :popup t))
(add-to-list 'shackle-rules '("\\*grep\\*" :regexp t :align t :size 0.5 :popup t :select t))
