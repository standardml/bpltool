using System;
using System.Collections.Generic;
using System.Text;

namespace CosmoBiz.EngineLibrary
{
  public class ActionSorter: IComparer<actionType>
  {
    // Seems to switch things around if there is no ordering... kinda unintended...
    // simple solution would be to sort twice? :) => really ugly tho....
    public int Compare(actionType a, actionType b)
    {

      if (a.priority == null)
      {
        if (b.priority != null)
          return 1;
        else
          return 0;
      }

      if (b.priority == null)
      {
        if (a.priority != null)
          return -1;
        else
          return 0;
      }

      if (a.priority == "")
      {
        if (b.priority != "")
          return 1;
        else
          return 0;
      }

      if (b.priority == "")
      {
        if (a.priority != "")
          return -1;
        else
          return 0;
      }

      if (Int32.Parse(a.priority) > Int32.Parse(b.priority))
        return -1;
      else if (Int32.Parse(a.priority) < Int32.Parse(b.priority))
        return 1;
      else return 0;
    }

    public void Sort(List<actionType> l)
    {
      l.Sort(this);
    }
  }
}
