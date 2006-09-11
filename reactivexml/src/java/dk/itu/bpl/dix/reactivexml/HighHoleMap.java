package dk.itu.bpl.dix.reactivexml;

import java.util.*;

public class HighHoleMap {
  private Hashtable<String, HighHole> map;
  
  public HighHoleMap() {
    map = new Hashtable<String, HighHole>();
  }
  
  public HighHoleMap(String name, HighHole hole) {
    this();
    put(name, hole);
  }
  
  public HighHole getHole(String name) {
    return map.get(name);
  }
  
  public void put(String name, HighHole hole) {
    map.put(name, hole);    
  }
  
  public static HighHoleMap join(HighHoleMap h1, HighHoleMap h2) {
    HighHoleMap newMap = new HighHoleMap();
    newMap.map.putAll(h1.map);
    newMap.map.putAll(h2.map);
    return newMap;
  }

}
