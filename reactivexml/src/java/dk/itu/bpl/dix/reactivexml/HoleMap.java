package dk.itu.bpl.dix.reactivexml;

import java.util.*;

import org.planx.xmlstore.*;

public class HoleMap<N extends Node> {

  private Hashtable<String, List<N>> holeMap;

  public Hashtable<String, List<N>> getMap() {
    return holeMap;
  }

  public HoleMap() {
    holeMap = new Hashtable<String, List<N>>();
  }

  public HoleMap(String holeName, N mapsTo) {
    this();
    List<N> nodeSet = new ArrayList<N>();
    nodeSet.add(mapsTo);
    holeMap.put(holeName, nodeSet);
  }

  public HoleMap(String holeName, List<N> mapsTo) {
    this();
    holeMap.put(holeName, mapsTo);
  }

  public List<N> getNodesMappedBy(String holeName) {
    return holeMap.get(holeName);
  }

  public void addHole(String holeName, List<N> mapsTo) throws HoleException {
    if (holeMap.containsKey(holeName) && !"ROOT_CHILD_HOLE".equals(holeName)) {
      List<N> nodes = holeMap.get(holeName);
      if (nodes.size() != mapsTo.size())
        throw new HoleException();
      List<N> tmp = new ArrayList<N>();
      tmp.addAll(mapsTo);
      for (Node n : nodes) {
        boolean isOk = false;
        for (int i = 0; i < tmp.size() && !isOk; i++) {
          if (n.contentEquals(tmp.get(i))) {
            isOk = true;
            tmp.remove(i);
          }
        }
        if (!isOk)
          throw new HoleException(holeName
              + " did match previosly stored nodes");
      }
    } else {
      holeMap.put(holeName, mapsTo);
    }
  }

  public void addHole(String holeName, N mapsTo) throws HoleException {
    List<N> nodeSet = new ArrayList<N>();
    nodeSet.add(mapsTo);
    addHole(holeName, nodeSet);
  }

  public static HoleMap join(HoleMap m1, HoleMap m2) throws HoleException {
    HoleMap newMap = new HoleMap();
    m1.addTo(newMap);
    m2.addTo(newMap);
    return newMap;
  }

  private HoleMap addTo(HoleMap<N> m) throws HoleException {
    Iterator<Map.Entry<String, List<N>>> it = holeMap.entrySet().iterator();
    while (it.hasNext()) {
      Map.Entry<String, List<N>> ent = it.next();
      m.addHole(ent.getKey(), ent.getValue());
    }
    return m;
  }
}