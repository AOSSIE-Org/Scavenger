package au.aossie.scavenger.model

import au.aossie.scavenger.expression.E

abstract class Model {

  // returns 'Some(true)' if 'e' is true in the model,
  //         'Some(false)' if 'e' is false in the model,
  //         'None' if the truth value of 'e' is unknown in the model.
  def truthValue(e: E): Option[Boolean]
}
