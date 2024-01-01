using System;
using System.Collections.Generic;
using System.Linq;

using System.Windows;
using System.Windows.Controls;

using FSharpCodeLib;
using Microsoft.FSharp.Collections;
using ScottPlot;

namespace Group_9_Project
{
    // Type for displaying in symlist
    class symListItem {
        public string Name { get; set; }
        public string Value { get; set; }
        public string Type { get; set; }
        public symListItem(string name, string type, string value)
        {
            this.Name = name;
            this.Type = type;
            this.Value = value;
        }
    }

    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        

        public Microsoft.FSharp.Collections.FSharpList<Tuple<string, lang.NumberType>> flist;
        public List<Tuple<string, lang.NumberType>> clist;
        public string BNF = "// Grammar in (E)BNF:\r\n <VA> ::= <varID> \"=\" <E>\r\n\r\n<E> ::= <T> <Eopt>" +
            "\r\n<Eopt> ::= \"+\" <T> <Eopt> | \"-\" <T> <Eopt> | <empty>\r\n\r\n<T> ::= <P> <Topt>\r\n" +
            "<Topt> ::= \"*\" <P> <Topt> | \"/\" <P> <Topt> | <empty>\r\n\r\n<P> ::= <C> <Popt>\r\n" +
            "<Popt> ::= \"^\" <NR> <Popt> | <empty>\r\n\r\n<C> ::= <NR> <Copt>\r\n" +
            "<Copt> ::= \"(\" <E> \")\" | <varID> | <empty>\r\n\r\n" +
            "<NR> ::= [\"IntNum\" | \"FloatNum\" | \"varVal\" ] <value> | \"(\" <E> \")\"\r\n" +
            "<varID> ::= [a-z,A-Z]+ \r\n\r\n(* varVal is fetched from symbol table/list with key varID *)\r\n";


        public enum CurrentMode {
            STANDARD = 1,
            PLOTTING = 2
         }

        public CurrentMode currentMode = CurrentMode.STANDARD;
        public bool Xfirst = true;

        public MainWindow()
        {
            InitializeComponent();
            tbErrOutput.Text = "...\n";
            clist = new List<Tuple<string, lang.NumberType>>();
            tbBNF.Text = BNF;
            scBNF.UpdateLayout();
            resetPlot();
        }

        private void Execute_Reset_Symtable_Click(object sender, RoutedEventArgs e)
        {
            this.clist = new List<Tuple<string, lang.NumberType>>();
            setDgSymList();
        }

        private void setDgSymList() {
            var symlist = new List<symListItem>();
            foreach (var item in clist)
            {
                var item1 = item.Item1;
                var item2 = item.Item2;

                if (item2.IsNT_INT) {
                    int val = lang.NumberType.getIntValue(item2);
                    symlist.Add(new symListItem(item.Item1, "Integer", val.ToString()));
                }
                else {
                    double val = lang.NumberType.getFloatValue(item2);
                    symlist.Add(new symListItem(item.Item1, "Floating Point", val.ToString()));
                }

                
            }
            dgSymlist.ItemsSource = symlist;
        }

        private void resetPlot() {
            WpfPlot1.Plot.Clear();
            WpfPlot1.Plot.XAxis.SetBoundary(-100, 100);
            WpfPlot1.Plot.YAxis.SetBoundary(-100, 100);

            var ch = WpfPlot1.Plot.AddCrosshair(0, 0);
            ch.Color = System.Drawing.Color.Gray;
            WpfPlot1.Refresh();
        }

        private void executeStandard() {
            ConsoleWriter consoleWriter = new ConsoleWriter(tbErrOutput);
            Console.SetOut(consoleWriter);

            var text = tbInput.Text;

            try {
                Tuple<string, FSharpList<Tuple<string, lang.NumberType>>> output = lang.main_wpf(text, true, ListModule.OfSeq(clist), "");
                var outputStr = output.Item1; // output string
                flist = output.Item2; // output sym list in F# syntax
                var ulist = addSymLists(clist, flist.ToList()); // union of current clist and new flist
                clist = ulist.ToList(); // setting clist to union list

                // if there is an output string, display. Else clear output field
                if (outputStr != null) { tbOutput.Text = outputStr.ToString(); }
                else { tbOutput.Text = ""; }

                Console.WriteLine("SYMLIST: " + ListModule.OfSeq(clist));


            }
            catch (Exception exception)
            {
                if (exception.Message == "Lexer error")
                {
                    Console.WriteLine("Lexer error! Please make sure you have the correct symbols in the query.");
                }
                else if (exception.Message == "Parser error")
                {
                    Console.WriteLine("Parser error! Please make sure you have the correct symbols in the query.");
                }
                else
                {
                    Console.WriteLine(exception.Message);
                }
            }
        }

        private void Execute_Button_Click(object sender, RoutedEventArgs e) {
            executeStandard();
            setDgSymList();
        }

        private List<Tuple<string, lang.NumberType>> addSymLists(List<Tuple<string, lang.NumberType>> inputList1, List<Tuple<string, lang.NumberType>> inputList2) {
            List<Tuple<string, lang.NumberType>> newList = inputList1;
            foreach (var tuple in inputList2)
            {
                if (!newList.Exists(x => x.Item1 == tuple.Item1))
                {
                    newList.Add(tuple);
                }
                else { 
                    newList.RemoveAll(x => x.Item1 == tuple.Item1);
                    newList.Add(tuple);
                }
            }

            return newList;
        }


        private void Clear_Button_Click(object sender, RoutedEventArgs e) {
            tbInput.Text = "";
            tbOutput.Text = "";
            tbErrOutput.Text = "...\n";

            resetPlot();
        }

        private void Quit_Button_Click(object sender, RoutedEventArgs e)
        {
            Application.Current.Shutdown();
        }

        private void ToggleHelpMessage(object sender, RoutedEventArgs e)
        {
            MessageBox.Show("This is the help text for our application. Here you will find the BNF and valid symbols to use in the query.", "Help Text", MessageBoxButton.OK);
        }

        private void tbInput_TextChanged(object sender, TextChangedEventArgs e)
         {
            tbInputPlaceholder.Visibility = tbInput.Text != "" ? Visibility.Hidden : Visibility.Visible;
        }


        private void switchMode(string str) {
            if (str != null)
            {
                switch (str)
                {
                    case "Standard":
                        currentMode = CurrentMode.STANDARD;
                        tcOutputTabs.SelectedItem = tcOutputTabs.Items.GetItemAt(0);
                        tcInputTabs.SelectedItem = tcInputTabs.Items.GetItemAt(0);
                        break;
                    case "Plotting":
                        currentMode = CurrentMode.PLOTTING;
                        tcOutputTabs.SelectedItem = tcOutputTabs.Items.GetItemAt(1);
                        tcInputTabs.SelectedItem = tcInputTabs.Items.GetItemAt(1);
                        break;

                    default:
                        break;
                }
            }
        }

        private void TabControl_Output_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            var header = ((TabItem)tcOutputTabs.SelectedItem).Header;
            string headerString = header.ToString() ?? "Standard";
            if (header != null)
            {
                switchMode(str: headerString);
            }
        }

        private void TabControl_Input_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            var header = ((TabItem)tcInputTabs.SelectedItem).Header;
            string headerString = header.ToString() ?? "Standard";
            if (header != null)
            {
                switchMode(str: headerString);
            }
        }

        private void Button_Switch_Xy_Click(object sender, RoutedEventArgs e)
        {
            Xfirst = !Xfirst;
            if (Xfirst) { 
                tbXyEquals.Text = "X = (";
                tbXy.Text = ") Y + (";
            }
            else {
                tbXyEquals.Text = "Y = (";
                tbXy.Text = ") X + (";
            }
        }

        private void Button_Execute_Plot_Click(object sender, RoutedEventArgs e)
        {
            var constInput = tbConstInput.Text;
            var coefInput = tbCoefInput.Text;
            var xpoints = new List<double>();
            var ypoints = new List<double>();

            for (int i = -100; i < 100; i++)
            {
                var j = lang.graphingFunction(coefInput, constInput, i.ToString(), ListModule.OfSeq(clist));
                if (Xfirst)
                {
                    xpoints.Add(i);
                    ypoints.Add(j);
                }
                else {
                    xpoints.Add(j);
                    ypoints.Add(i);
                }
            }

            WpfPlot1.Plot.AddScatter(xpoints.ToArray(), ypoints.ToArray());
            WpfPlot1.Refresh();
        }

        private void Execute_Run_Tests_Click(object sender, RoutedEventArgs e)
        {
            ConsoleWriter consoleWriter = new ConsoleWriter(tbErrOutput);
            Console.SetOut(consoleWriter);

            var res = lang.runTests;

        }
    }
}
