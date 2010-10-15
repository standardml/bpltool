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
    public partial class principalvis : System.Web.UI.Page
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
            if (Session["processId"] != null && Session["principal"] != null)
            {
                string principal = (string)Session["principal"];

                Bitmap image = visualizer.VisualizePrincipalView(principal);

                Response.ContentType = "image/jpeg";
                image.Save(Response.OutputStream, ImageFormat.Jpeg);
            }

            /*
            //int processId = Request.QueryString("processId")  
            //if (Request.QueryString["processId"] != null)
            if (Session["processId"] != null && Session["principal"] != null)
            {
                //int processId = Int16.Parse(Request.QueryString["processId"]);

                string processInstanceXml;
                int processId = (int)Session["processId"];
                string principal = (string)Session["principal"];               
                if (Session["processInstanceId"] != null)
                {
                    int processInstanceId = (int)Session["processInstanceId"];
                    processInstanceXml = RemoteServicesHandler.GetProcessInstance(processId, processInstanceId);                    
                }
                else
                {
                    processInstanceXml = RemoteServicesHandler.GetProcess(processId);                    
                }

                
                

                DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);
                

                Bitmap image = Visualizer.VisualizePrincipalView(DCRSProcessInstance, principal);

                Response.ContentType = "image/jpeg";
                image.Save(Response.OutputStream, ImageFormat.Jpeg);
            }
            */
            
        }
    }
}