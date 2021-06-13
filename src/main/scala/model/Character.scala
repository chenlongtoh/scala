package model

import enumeration.Roles;

class Character(val role: Roles.Role) {

  def isAdult(): Boolean = {
    return role == Roles.Father || role == Roles.Mother || role == Roles.Police;
  }

  def isCriminal(): Boolean = return role == Roles.Criminal

  override def toString(): String = {
    return role.toString();
  }

}
