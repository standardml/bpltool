using System.Collections.Generic;
using System.Windows.Forms;

namespace DCRSModelCheckerUI.HelperClasses
{
    class ThreadSafeCallHandler
    {

        #region Delegates

        delegate void SetControlTextCallback(Control control, string text);


        private delegate void AddItemsToListControlCallback(ListBox listboxControl, object[] items);


        private delegate void AddItemsToListViewControlCallback(ListView listview, ListViewItem[] items);

        private delegate void AddItemsToListViewControlWithGroupsCallback(
            ListView listview, ListViewItem[] items, ListViewGroup group);



        #endregion



        #region Set Text to Controls, Forms, group boxes.

        public static void SetControlText(Control control, string text)
        {
            // InvokeRequired required compares the thread ID of the
            // calling thread to the thread ID of the creating thread.
            // If these threads are different, it returns true.
            if (control.InvokeRequired)
            {
                var d = new SetControlTextCallback(SetControlText);

                control.Invoke(d, new object[] { control, text });
            }
            else
            {
                control.Text = text;
            }
        }

        public static void AddItemsToListControl(ListBox control, object[] items)
        {

           // InvokeRequired required compares the thread ID of the
            // calling thread to the thread ID of the creating thread.
            // If these threads are different, it returns true.
            if (control.InvokeRequired)
            {
                var d = new AddItemsToListControlCallback(AddItemsToListControl);

                control.Invoke(d, new object[] { control, items });
            }
            else
            {
                // First clear the items.
                control.Items.Clear();

                control.Items.AddRange(items);
            }

        }

        public static void AddItemsToListViewControl(ListView control, ListViewItem[] items)
        {

            // InvokeRequired required compares the thread ID of the
            // calling thread to the thread ID of the creating thread.
            // If these threads are different, it returns true.
            if (control.InvokeRequired)
            {
                var d = new AddItemsToListViewControlCallback(AddItemsToListViewControl);

                control.Invoke(d, new object[] { control, items });
            }
            else
            {
                // First clear the items.
                control.Items.Clear();

                control.Items.AddRange(items);
            }

        }


        public static void AddItemsToListViewControlWithGroups(ListView control, ListViewItem[] items, ListViewGroup group)
        {

            // InvokeRequired required compares the thread ID of the
            // calling thread to the thread ID of the creating thread.
            // If these threads are different, it returns true.
            if (control.InvokeRequired)
            {
                var d = new AddItemsToListViewControlWithGroupsCallback(AddItemsToListViewControlWithGroups);

                control.Invoke(d, new object[] {control, items, group});
            }
            else
            {
                // First add the group to list view.
                control.Groups.Add(group);

                control.Items.AddRange(items);
            }

        }


        #endregion






        

        private delegate void UpdateListboxDictionaryCallback(ListBox listBox, Dictionary<int, string> listValues);

        private delegate void UpdateListboxActionDictionaryCallback(ListBox listBox, Dictionary<short , string> listValues);

        private delegate void UpdateListWithListCallback(ListBox listBox, Dictionary<int, string> listValues);

        

        public static void UpdateListWithProcessDictionaryValues(ListBox listBox, Dictionary<int, string> listValues)
        {

            // InvokeRequired required compares the thread ID of the
            // calling thread to the thread ID of the creating thread.
            // If these threads are different, it returns true.
            if (listBox.InvokeRequired)
            {
                var d = new UpdateListboxDictionaryCallback(UpdateListWithProcessDictionaryValues);

                listBox.Invoke(d, new object[] { listBox, listValues });
            }
            else
            {
                // Clear all the items.
                listBox.Items.Clear();

                foreach (var keyValuePair in listValues)
                {
                    listBox.Items.Add(new ProcessItem { Name = string.Format("{0}_{1}", keyValuePair.Key, keyValuePair.Value), ProcessId = keyValuePair.Key });
                }

            }
        }

        public static void UpdateListWithListValues(ListBox listBox, List<int> listValues)
        {

            // InvokeRequired required compares the thread ID of the
            // calling thread to the thread ID of the creating thread.
            // If these threads are different, it returns true.
            if (listBox.InvokeRequired)
            {
                var d = new UpdateListWithListCallback(UpdateListWithProcessDictionaryValues);

                listBox.Invoke(d, new object[] { listBox, listValues });
            }
            else
            {
                // Clear all the items.
                listBox.Items.Clear();

                foreach (var item in listValues)
                {
                    listBox.Items.Add(item);
                }

            }
        }

        public static void UpdateListWithActionDictionaryValues(ListBox listBox, Dictionary<short, string> listValues)
        {

            // InvokeRequired required compares the thread ID of the
            // calling thread to the thread ID of the creating thread.
            // If these threads are different, it returns true.
            if (listBox.InvokeRequired)
            {
                var d = new UpdateListboxActionDictionaryCallback(UpdateListWithActionDictionaryValues);

                listBox.Invoke(d, new object[] { listBox, listValues });
            }
            else
            {
                // Clear all the items.
                listBox.Items.Clear();

                foreach (var keyValuePair in listValues)
                {
                    listBox.Items.Add(new ActionInfo {Name = keyValuePair.Value, Id = keyValuePair.Key});
                }

            }
        }

    }
}
