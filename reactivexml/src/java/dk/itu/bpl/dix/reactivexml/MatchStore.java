package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.apache.log4j.Logger;

public class MatchStore {
  private static final Logger logger = Logger.getLogger(MatchStore.class);
  private Hashtable<String, String> store;

  public MatchStore() {
    store = new Hashtable<String, String>();
  }

  public MatchStore(Hashtable<String, String> store) {
    this.store = store;
  }

  public String getVariable(String name) {
    return store.get(name);
  }

  public boolean hasVariable(String name) {
    Set<String> keys = store.keySet();
    for (String key : keys) {
      if (key.equals(name))
        return true;
    }
    return false;
    //return store.containsKey(name);
  }

  public void addVariable(String name, String value) {
    logger.debug("Adding variable: " + name + " : " + value);
    store.put(name, value);
  }

  public Set<String> getVariables() {
    return store.keySet();
  }

  public MatchStore copy() {
    return new MatchStore((Hashtable<String, String>) this.store.clone());
  }

  public static MatchStore join(MatchStore m1, MatchStore m2) {
    //TODO if both store contains the same key, then this key will be copied twice.
    Hashtable<String, String> newM = new Hashtable<String, String>();
    newM.putAll(m1.store);
    newM.putAll(m2.store);
    MatchStore res = new MatchStore(newM);
    return res;
  }

  public String toString() {
    return store.toString();
  }
}