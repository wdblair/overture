staload "rat.sats"
staload "flow.sats"

extern fun controller(i: strict_flow (int, 100, RationalDiv(0, 1)), j: strict_flow (int, 100, RationalDiv(0, 1))): (strict_flow (int, 100, RationalDiv(0, 1)), strict_flow (int, 100, RationalDiv(0, 1)))

extern fun database(i: strict_flow (int, 10, RationalDiv(0, 1))): (strict_flow (int, 10, RationalDiv(0, 1)))

fun main(i: strict_flow (int, 10, RationalDiv(0, 1))): (strict_flow (int, 100, RationalDiv(0, 1))) = let
var response : strict_flow (int, 10, RationalDiv(0, 1))
prval pfresponse = flow_future_make (response)
var command : strict_flow (int, 100, RationalDiv(0, 1))
prval pfcommand = flow_future_make (command)
val (o, command') = controller ((flow_divide_clock (i, 10)), (flow_divide_clock ((flow_fby (0, response)), 10)))
val (response') = database (flow_multiply_clock (command, 10))

prval () = flow_future_elim (pfresponse, response, response')
prval () = flow_future_elim (pfcommand, command, command')

in
(o)
end


