using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.ServiceModel;
using System.Text;
using System.Windows.Forms;
using DCRSModelCheckerUI.Services;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace DCRSModelCheckerUI
{
    public partial class FormMain : Form
    {

        #region static variables.

        private static readonly FormMain formMdiParent;
        
        private static FormSpecification specificationForm;

        private static FormProcessRepository formProcessRepository;
        
        #endregion



        #region Constructor Logic.
        private FormMain()
        {
            InitializeComponent();

            menuStrip1.MdiWindowListItem = windowToolStripMenuItem;

            HostServices();

        }

        static FormMain()
        {
            formMdiParent = new FormMain();
        }



        #endregion


        #region Static Methods.

        
        public static FormMain GetMdiParent()
        {
            return formMdiParent;
        }

        public static FormSpecification GetSpecificationForm()
        {
            if ((specificationForm != null) && (!specificationForm.IsDisposed)) return specificationForm;


            InitializeSpecificationForm();

            return specificationForm;

        }

        public static FormProcessRepository GetProcessRepositoryForm()
        {

            if ((formProcessRepository == null) || (formProcessRepository.IsDisposed))
            {
                formProcessRepository = new FormProcessRepository {MdiParent = GetMdiParent()};
            }

            return formProcessRepository;
        }


        #endregion




        



        private static void InitializeSpecificationForm()
        {
            specificationForm = new FormSpecification();

            specificationForm.Show();

            specificationForm.BringToFront();

        }


        private void openDCRSSpecToolStripMenuItem_Click(object sender, EventArgs e)
        {
            var formSpecification = GetSpecificationForm();

            

            formSpecification.Show();

            formSpecification.BringToFront();


        }

        private void HostServices()
        {
            try
            {
                //HostServices();

                RemoteServicesHandler.HostSubscriptionServiceClient();


                RemoteServicesHandler.HostNotificationService();


            }
            catch (Exception exception)
            {

                MessageBox.Show(exception.Message);
            }


        }

        private void hostServicesToolStripMenuItem_Click(object sender, EventArgs e)
        {

            HostServices();
        }

        private void ProcessListStripMenuItem_Click(object sender, EventArgs e)
        {

            ShowForm(GetProcessRepositoryForm());
        }


        #region static Functions

        

        private static void ShowForm(Form formToshow)
        {
            formToshow.Show();

            formToshow.BringToFront();

            return;
        }


        #endregion
    }
}
