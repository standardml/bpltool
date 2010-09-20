/*
 * Limited implemetation of a set class, because the class that I used for creating the forcebased algorithm had copyright notices that we may want to get around, 
 * so I mde my own implementation with only the funcionality that we need in this case. Feel free to exted however :) - Tijs 
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ITU.DK.DCRS.Visualization
{
  public class Set<T> : ICollection<T>, IEnumerable<T>
  {
    private Dictionary<T, bool> contents;

    public Set()
    {
      contents = new Dictionary<T, bool>();
    }

    #region ICollection<T> Members

    public void Add(T item)
    {
      contents.Add(item, true);
    }

    public void Clear()
    {
      contents.Clear();
    }

    public bool Contains(T item)
    {
      return contents.ContainsKey(item);
    }

    public void CopyTo(T[] array, int arrayIndex)
    {
      throw new NotImplementedException();
    }

    public int Count
    {
      get { return contents.Count; }
    }

    public bool IsReadOnly
    {
      get { throw new NotImplementedException(); }
    }

    public bool Remove(T item)
    {
      return contents.Remove(item);
    }

    #endregion

    #region IEnumerable<T> Members

    public IEnumerator<T> GetEnumerator()
    {
      return contents.Keys.GetEnumerator();
    }

    #endregion

    #region IEnumerable Members

    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
    {
      return contents.Keys.GetEnumerator();
    }

    #endregion

    #region IEnumerable<T> Members

    IEnumerator<T> IEnumerable<T>.GetEnumerator()
    {
      return contents.Keys.GetEnumerator();
    }

    #endregion
    
    #region Set operators

    public Set<T> Difference(IEnumerable<T> b)
    {      
      Set<T> result = new Set<T>();
      foreach (T e in this)
        if (!b.Contains(e))
          result.Add(e);
      return result;
    }

    #endregion
  }
}
