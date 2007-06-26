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
        private Point exitPoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 2, 10);
        private Point startPoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 2, 10);
        private Point splitPoint;
        public Point StartPoint
        {
            get { return startPoint; }
        }


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

        public List<DrawableObject> DrawableObjectList;

        bool flow = false;

        public Draw()
        {
            exitPoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 2, 10);

            img = new Bitmap(Screen.PrimaryScreen.WorkingArea.Width, 8000);
            graph = Graphics.FromImage(img);
        }

        public Bitmap InterpretElements(ListedElements elements)
        {
            DrawableObjectList = new List<DrawableObject>();

            bool changeSide = false;

            Side currentSide = Side.center;
            Point leftDrawPoint = new Point(0, 0);
            Point rightDrawPoint = new Point(0, 0);
            Point splitPoint = exitPoint;
            startPoint = exitPoint;

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
                        DrawSplit(exitPoint);
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

                        DrawJoin(exitPoint);

                        break;

                    default:
                        break;
                }
            }
            DrawElements();

            return img;
        }

        public void DrawElements()
        {
            startPoint = exitPoint;
            graph.Clear(Color.Transparent);
            foreach (DrawableObject obj in DrawableObjectList)
            {
                if (obj.draw)
                {
                    startPoint = obj.Draw(startPoint);
                }
            }

        }

        private void DrawBox(Point point, Side side, string name, string owner)
        {
            int x = 0;
            switch (side)
            {
                case Side.left:
                    x = Screen.PrimaryScreen.WorkingArea.Width / 4;
                    break;
                case Side.right:
                    x = (Screen.PrimaryScreen.WorkingArea.Width / 4) * 3;
                    break;
                case Side.center:
                    x = Screen.PrimaryScreen.WorkingArea.Width / 2;
                    break;
                default:
                    break;
            }

            Box box = new Box(point, graph, name, owner, x);
            DrawableObjectList.Add(box);

            //exitPoint.Y = point.Y + box.Height + box.Linelength;

        }

        private void DrawSplit(Point point)
        {
            int length = Screen.PrimaryScreen.WorkingArea.Width / 2;
            Split split = new Split(graph, length);

            split.TopPoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 4, point.Y);
            split.BottomPoint = new Point((Screen.PrimaryScreen.WorkingArea.Width / 4) * 3, point.Y + split.LineLength);

            DrawableObjectList.Add(split);

            //exitPoint.Y = point.Y + split.LineLength;
        }

        private void DrawJoin(Point point)
        {
            int length = Screen.PrimaryScreen.WorkingArea.Width / 2;

            Point leftpoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 4, point.Y);
            Point rightpoint = new Point((Screen.PrimaryScreen.WorkingArea.Width / 4) * 3, point.Y);

            Join join = new Join(graph, length);


            DrawableObjectList.Add(join);



            //exitPoint.Y = point.Y + join.LineLength;
        }


        private void DrawFlowAdjustment(Point startPoint, Point endPoint)
        {
            Pen pen = new Pen(Color.Black);
            graph.DrawLine(pen, startPoint.X, startPoint.Y, endPoint.X, endPoint.Y);

            //exitPoint.Y = endPoint.Y;
        }

    }
}
