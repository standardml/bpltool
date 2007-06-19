using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.Windows.Forms;

namespace CFDemo
{
    class Draw
    {

        private enum Side { left, right, center };
        private Point exitPoint;

        public int PointX
        {
            get { return exitPoint.X; }
            set { exitPoint.X = value; }
        }

        public int PointY
        {
            get { return exitPoint.Y; }
            set { exitPoint.Y = value; }
        }

        private Bitmap img;
        private Graphics graph;

        public List<Box> BoxList;
        public List<Split> SplitList;


        public Draw()
        {
            exitPoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 2, 10);

            img = new Bitmap(Screen.PrimaryScreen.WorkingArea.Width, 9000);
            graph = Graphics.FromImage(img);
        }
        bool flow = false;
        public Bitmap DoDraw(ListedElements elements)
        {
            BoxList = new List<Box>();
            SplitList = new List<Split>();
            Point splitPoint = exitPoint;
            bool changeSide = false;

            Side currentSide = Side.center;
            Point leftDrawPoint = new Point(0, 0);
            Point rightDrawPoint = new Point(0, 0);
            exitPoint.X = Screen.PrimaryScreen.WorkingArea.Width / 2;
            exitPoint.Y = 10;
            graph.Clear(Color.Transparent);

            foreach (Element elem in elements)
            {
                switch (elem.Type)
                {
                    case "sequence":
                        if (flow && currentSide == Side.center) //Betyder at seq er første i et flow
                        {
                            currentSide = Side.left;
                        }

                        else if (flow && currentSide == Side.left) //Betyder at seq er anden i et flow
                        {
                            currentSide = Side.right;
                        }

                        else if (flow && currentSide == Side.right) //Betyder at seq er første efter et flow
                        {
                            flow = false;
                            currentSide = Side.center;
                        }


                        break;

                    case "endsequence":
                        if (flow && currentSide == Side.left)
                        {
                            changeSide = true;
                            leftDrawPoint.Y = exitPoint.Y;
                            leftDrawPoint.X = Screen.PrimaryScreen.WorkingArea.Width / 4;
                            break;
                        }
                        if (flow && currentSide == Side.right)
                        {
                            rightDrawPoint.Y = exitPoint.Y;
                            rightDrawPoint.X = (Screen.PrimaryScreen.WorkingArea.Width / 4) * 3;
                            break;
                        }
                        break;

                    case "activity":
                        if (changeSide)
                        {
                            DrawBox(splitPoint, currentSide, elem.Name, elem.Owner);
                            changeSide = false;
                        }
                        else
                            DrawBox(exitPoint, currentSide, elem.Name, elem.Owner);

                        break;
                    case "Flow":

                        splitPoint.Y = exitPoint.Y + 31;
                        DrawSplit(exitPoint, currentSide);
                        flow = true;

                        break;

                    case "endFlow":
                        flow = false;
                        currentSide = Side.center;
                        if (leftDrawPoint.Y < rightDrawPoint.Y)
                        {                                  
                            DrawFlowAdjustment(leftDrawPoint, new Point(leftDrawPoint.X, rightDrawPoint.Y));
                        }
                        else if (leftDrawPoint.Y > rightDrawPoint.Y)
                        {
                            DrawFlowAdjustment(rightDrawPoint, new Point(rightDrawPoint.X, leftDrawPoint.Y));
                        }

                        DrawJoin(exitPoint, currentSide);

                        break;

                    default:
                        break;
                }
            }
            DrawAll();
            
            return img;
        }

        public void DrawAll()
        {
            foreach (Box box in BoxList)
            {
                box.Draw();
            }
            foreach (Split split in SplitList)
            {
                split.Draw();

            }
        }

        private void DrawBox(Point point, Side side, string name, string owner)
        {
            switch (side)
            {
                case Side.left:
                    point.X = Screen.PrimaryScreen.WorkingArea.Width / 4;
                    break;
                case Side.right:
                    point.X = (Screen.PrimaryScreen.WorkingArea.Width / 4) * 3;
                    break;
                case Side.center:
                    point.X = Screen.PrimaryScreen.WorkingArea.Width / 2;
                    break;
                default:
                    break;
            }

            Box box = new Box(graph, point, name, owner);
            BoxList.Add(box);
            //box.Draw();

            exitPoint.Y = point.Y + box.Height + box.Linelength;

        }

        private void DrawSplit(Point point, Side side)
        {
            Split split = new Split(graph,point);

            split.TopPoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 4, point.Y);
            split.BottomPoint = new Point((Screen.PrimaryScreen.WorkingArea.Width / 4) * 3, point.Y + split.LineLength);
            

            //Graphics graph = CreateGraphics();
            //graph.DrawLine(pen, point.X, point.Y, point.X, point.Y + linelength);
            //graph.DrawLine(pen, leftpoint.X, leftpoint.Y, rightpoint.X, rightpoint.Y);

            SplitList.Add(split);

            exitPoint.Y = point.Y + split.LineLength;
        }

        private void DrawJoin(Point point, Side side)
        {
            Pen pen = new Pen(Color.Black);
            int linelength = 30;
            Point leftpoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 4, point.Y + linelength);
            Point rightpoint = new Point((Screen.PrimaryScreen.WorkingArea.Width / 4) * 3, point.Y + linelength);
            //Graphics graph = CreateGraphics();
            graph.DrawLine(pen, leftpoint.X, point.Y, leftpoint.X, leftpoint.Y);
            graph.DrawLine(pen, rightpoint.X, point.Y, rightpoint.X, rightpoint.Y);
            graph.DrawLine(pen, leftpoint.X, leftpoint.Y, rightpoint.X, rightpoint.Y);


            exitPoint.Y = point.Y + linelength;
        }

        private void DrawFlowAdjustment(Point startPoint, Point endPoint)
        {
            Pen pen = new Pen(Color.Black);
            graph.DrawLine(pen, startPoint.X, startPoint.Y, endPoint.X, endPoint.Y);

            exitPoint.Y = endPoint.Y;
        }
    }
}
