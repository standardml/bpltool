package dk.itu.bpl.dix.reactivexml;

import java.util.Hashtable;

public class ContextHoleMap {

  private Hashtable<String, ContextHoleMatch> map;

  public ContextHoleMap() {
    map = new Hashtable<String, ContextHoleMatch>();
  }

  public ContextHoleMap(String name, ContextHoleMatch hole) {
    this();
    if(name == null) 
      throw new IllegalArgumentException("Name cannot be null");
    if(hole == null) 
      throw new IllegalArgumentException("ContextHoleMatch cannot be null");
    put(name, hole);
  }

  public void put(String name, ContextHoleMatch hole) {
    map.put(name, hole);
  }

  public ContextHoleMatch getContextHole(String name) {
    return map.get(name);
  }

  public static ContextHoleMap join(ContextHoleMap h1, ContextHoleMap h2) {
    ContextHoleMap newMap = new ContextHoleMap();
    newMap.map.putAll(h1.map);
    newMap.map.putAll(h2.map);
    return newMap;
  }
}
