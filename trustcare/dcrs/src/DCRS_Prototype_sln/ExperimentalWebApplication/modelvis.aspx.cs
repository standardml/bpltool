using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Drawing;
using System.Drawing.Imaging;
using System.Drawing.Drawing2D;
using ExperimentalWebApplication.Services;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.Visualization;
namespace ExperimentalWebApplication
{
    public partial class modelvis : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {

                //int processId = Request.QueryString("processId")  
            //if (Request.QueryString["processId"] != null)
            if (Session["processId"] != null)
            {
                //int processId = Int16.Parse(Request.QueryString["processId"]);
                int processId = (int)Session["processId"];                
                var processInstanceXml = RemoteServicesHandler.GetProcess(processId);

                DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);

                Bitmap image = Visualizer.Visualize(DCRSProcessInstance.Specification);

                Response.ContentType = "image/jpeg";
                image.Save(Response.OutputStream, ImageFormat.Jpeg);
            }
            
        }
    }
}