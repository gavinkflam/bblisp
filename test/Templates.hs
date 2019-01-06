module Templates
    (
      -- Templates
      sampleTemplate1
    ) where

import Data.List (intercalate)

-- Sample template containing all kind of tokens.
sampleTemplate1 :: String
sampleTemplate1 = intercalate "\n"
    [ "{{# if $name }}Hello {{ $name }}.{{/#}}"
    , "{{# unless $name }}Hello world.{{/#}}"
    , "{{# with $params }}{{# if $$n }}{{ $$n }}{{/#}}{{# unless $$n }}1{{/#}}{{/#}}"
    , "{{! I am invisible }}"
    , "The answer is {{ + (- 50 20) 12 }}."
    , "First 10 digits of {{ \"pi\" }} is {{ 3.1415926535 }}."
    ]
