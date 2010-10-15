using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Drawing;
using System.Drawing.Imaging;
using System.Drawing.Drawing2D;
using ITU.DK.DCRS.RemoteServices;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.Visualization;
namespace DCRSWebUI
{
    public partial class modelvis : System.Web.UI.Page
    {

        private Visualizer visualizer
        {
            get
            {
                return (Visualizer)Session["visualizer"];
            }
        }

        protected void Page_Load(object sender, EventArgs e)
        {

                //int processId = Request.QueryString("processId")  
            //if (Request.QueryString["processId"] != null)
            if (Session["processId"] != null)
            {
                //int processId = Int16.Parse(Request.QueryString["processId"]);


                Bitmap image = visualizer.Visualize();

                Response.ContentType = "image/jpeg";
                image.Save(Response.OutputStream, ImageFormat.Jpeg);

                Response.ContentType = "image/jpeg";
                image.Save(Response.OutputStream, ImageFormat.Jpeg);
            }
            
        }
    }
}