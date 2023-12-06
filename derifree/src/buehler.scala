package derifree
package buehler

def strikeToPureStrike(strike: Double, forward: Double, divFloor: Double): Double =
  (strike - divFloor) / (forward - divFloor)

def pureStrikeToStrike(pureStrike: Double, forward: Double, divFloor: Double): Double =
  pureStrike * (forward - divFloor) + divFloor
