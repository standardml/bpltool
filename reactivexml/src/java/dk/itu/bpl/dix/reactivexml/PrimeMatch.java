package dk.itu.bpl.dix.reactivexml;

import org.planx.xmlstore.*;

public class PrimeMatch {
  private Match match;
  private Node location;
  private Node wide;

  public PrimeMatch(Match match, Node location, Node wide) {
    this.match = match;
    this.location = location;
    this.wide = wide;
  }

  public Match getMatch() {
    return match;
  }

  public Node getLocation() {
    return location;
  }

  public Node getWide() {
    return wide;
  }
}