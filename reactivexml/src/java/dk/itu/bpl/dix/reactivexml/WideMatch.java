package dk.itu.bpl.dix.reactivexml;

import java.util.*;
import org.planx.xmlstore.*;

public class WideMatch {

  private List<Node> wides;
  private List<PrimeMatch> matches;
  private MatchStore store;
  private HoleMap holeMap;
  private ContextHoleMap conMap;

  public WideMatch() {
    matches = new ArrayList<PrimeMatch>();
    store = new MatchStore();
    holeMap = new HoleMap<Node>();
    wides = new ArrayList<Node>();
    conMap = new ContextHoleMap();
  }

  public WideMatch(MatchStore store) {
    this();
    this.store = store;
  }

  public WideMatch(List<PrimeMatch> matches, List<Node> wides,
      MatchStore store, HoleMap holeMap, ContextHoleMap conMap) {
    this.matches = matches;
    this.wides = wides;
    this.store = store;
    this.holeMap = holeMap;
    this.conMap = conMap;
  }

  public WideMatch(Node wide, PrimeMatch match) {
    matches = new ArrayList<PrimeMatch>();
    matches.add(match);
    store = match.getMatch().getMatchStore();
    holeMap = match.getMatch().getHoleMap();
    wides = new ArrayList<Node>();
    wides.add(match.getWide());
    conMap = match.getMatch().getContextHoleMap();

  }

  public static WideMatch join(WideMatch arg1, WideMatch arg2)
      throws HoleException {
    List<Node> wides = new ArrayList<Node>();
    wides.addAll(arg1.getWides());
    wides.addAll(arg2.getWides());
    List<PrimeMatch> matches = new ArrayList<PrimeMatch>();
    matches.addAll(arg1.getPrimeMatches());
    matches.addAll(arg2.getPrimeMatches());
    MatchStore store = MatchStore.join(arg1.getMatchStore(), arg2
        .getMatchStore());
    HoleMap map = HoleMap.join(arg1.getHoleMap(), arg2.getHoleMap());
    ContextHoleMap conMap = ContextHoleMap.join(arg1.conMap, arg2.conMap);
    return new WideMatch(matches, wides, store, map, conMap);

  }

  public ContextHoleMap getContextHoleMap() {
    return conMap;
  }

  public List<Node> getWides() {
    return wides;
  }

  public MatchStore getMatchStore() {
    return store;
  }

  public HoleMap getHoleMap() {
    return holeMap;
  }

  public List<PrimeMatch> getPrimeMatches() {
    return matches;
  }

}
