module Main where

import Weyl as W
import SemiSimple as SS

main = print $ length . elements . SS.weylGroup $ fromSimples 0 [F4]
