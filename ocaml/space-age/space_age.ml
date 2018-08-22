open Base

type planet =
  | Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Neptune
  | Uranus
;;

let period planet years = match planet with
  | Mercury -> years /. 0.2408467
  | Venus -> years /. 0.61519726
  | Earth -> years
  | Mars -> years /. 1.8808158
  | Jupiter -> years /. 11.862615
  | Saturn -> years /. 29.447498
  | Uranus -> years /. 84.016846
  | Neptune -> years /. 164.79132
;;

let age_on planet age =
  let seconds = Float.of_int age in
  period planet (seconds /. 31557600.0)

