using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.Samples;
using ITU.DK.DCRS.Visualization;
using System.Drawing;
using System.Drawing.Imaging;

namespace DCRSWebUI
{
    public partial class Givemedecine : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            DCRSSpecification gmspec = DCRSSampleModels.GetGiveMedicineSpecification();
            //Bitmap img = Visualizer.Visualize(gmspec);
            //img.Save("C:\\Documents and Settings\\jadu\\My Documents\\Telechargements\\DCRS_Prototype_sln\\DCRS_Prototype_sln\\DCRSWebUI\\tmp\\img.jpg",ImageFormat.Jpeg);

        }
    }
}