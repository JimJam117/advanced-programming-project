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
        // global lists
        public Microsoft.FSharp.Collections.FSharpList<Tuple<string, lang.NumberType>> flist; // the fsharp version of the global list
        public List<Tuple<string, lang.NumberType>> clist; // the csharp version of the global list
        
        // String version of the BNF
        public string BNF = "" +
            "BNF:\r\n<VA> ::= <varID> \"=\" <E>\r\n\r\n<E> ::= <T> <Eopt>" +
            "\r\n<Eopt> ::= \"+\" <T> <Eopt> | \"-\" <T> <Eopt> | <empty>\r\n\r\n<T> ::= <P> <Topt>\r\n" +

            "<Topt> ::= \"*\" <P> <Topt> | \"/\" <P> <Topt> | \"(\" <E> \")\" | <varID> | <empty>\r\n\r\n<P> ::= <NR> <Popt>\r\n" +
            "<Popt> ::= \"^\" <NR> <Popt> | <empty>\r\n\r\n" +

            "<NR> ::= \"-\"[\"IntNum\" | \"FloatNum\" | \"RatNum\" | \"varVal\" ] <value> | "+
            "[\"IntNum\" | \"FloatNum\" | \"RatNum\" | \"varVal\" ] <value> | \"(\" <E> \")\" | \"-\"(\" <E> \")\"\r\n\r\n" +

            "<varID> ::= [a-z,A-Z]+ \r\n(* varVal is fetched from symbol table/list with key varID *)\r\n";

        // Enum for application mode - either STANDARD or PLOTTING
        public enum CurrentMode {
            STANDARD = 1,
            PLOTTING = 2
         }

        // Start in STANDARD mode
        public CurrentMode currentMode = CurrentMode.STANDARD;

        // This is used to determine if linear function is (x = ay + b) or (y = ax + b)
        public bool Xfirst = true;

        // ctor for main window - executed on startup
        public MainWindow()
        {
            InitializeComponent();
            tbErrOutput.Text = "...\n";
            clist = new List<Tuple<string, lang.NumberType>>();
            tbBNF.Text = BNF;
            scBNF.UpdateLayout();
            resetPlot();
        }

        // ------------ HELPER FUNCTIONS--------------------

        /// <summary>
        /// Helper function <c>Execute_Reset_Symtable_Click</c> will clear the symbol table.
        /// </summary>
        private void handleInterpreterExceptions(Exception exception) {
            if (exception.Message == "Lexer error")
            {
                var msg = "Please make sure you have the correct symbols in the query.";
                MessageBox.Show("Exception: " + exception.Message + "\n" + msg, "Exception!", MessageBoxButton.OK, MessageBoxImage.Error);
                Console.WriteLine(exception.Message + " - " + msg);
            }
            else if (exception.Message == "Parser error")
            {
                var msg = "Please make sure you have the correct symbols in the query.";
                MessageBox.Show("Exception: " + exception.Message + "\n" + msg, "Exception!", MessageBoxButton.OK, MessageBoxImage.Error);
                Console.WriteLine(exception.Message + " - " + msg);
            }
            else
            {
                MessageBox.Show(exception.Message, "Exception!", MessageBoxButton.OK, MessageBoxImage.Error);
                Console.WriteLine(exception.Message);
            }
        }

        /// <summary>
        /// Helper function <c>setDgSymList</c> builds a list of symbols from the items in clist and then sets the data grid to this list.
        /// </summary>
        private void setDgSymList() {
            var symlist = new List<symListItem>();
            foreach (var item in clist)
            {
                if (item.Item2.IsINTEGER)
                {
                    int val = lang.NumberType.getIntValue(item.Item2);
                    symlist.Add(new symListItem(item.Item1, "Integer", val.ToString()));
                }
                else if (item.Item2.IsRATIONAL) {
                    var val = lang.NumberType.getRatValue(item.Item2);
                    symlist.Add(new symListItem(item.Item1, "Rational", val.Item1.ToString() + "\\" + val.Item2.ToString()));
                }

                else
                {
                    double val = lang.NumberType.getFloatValue(item.Item2);
                    symlist.Add(new symListItem(item.Item1, "Floating Point", val.ToString()));
                }  
            }
            dgSymlist.ItemsSource = symlist;
        }

        /// <summary>
        /// Helper function <c>resetPlot</c> simply resets the plotting area.
        /// </summary>
        private void resetPlot() {
            WpfPlot1.Plot.Clear();
            WpfPlot1.Plot.XAxis.SetBoundary(-100, 100);
            WpfPlot1.Plot.YAxis.SetBoundary(-100, 100);

            var ch = WpfPlot1.Plot.AddCrosshair(0, 0);
            ch.Color = System.Drawing.Color.Gray;
            WpfPlot1.Refresh();
        }

        /// <summary>
        /// Helper function <c>addSymLists</c> simply adds two sym lists together and returns the result.
        /// </summary>
        private List<Tuple<string, lang.NumberType>> addSymLists(List<Tuple<string, lang.NumberType>> inputList1, List<Tuple<string, lang.NumberType>> inputList2)
        {
            List<Tuple<string, lang.NumberType>> newList = inputList1;
            foreach (var tuple in inputList2)
            {
                if (!newList.Exists(x => x.Item1 == tuple.Item1))
                {
                    newList.Add(tuple);
                }
                else
                {
                    newList.RemoveAll(x => x.Item1 == tuple.Item1);
                    newList.Add(tuple);
                }
            }

            return newList;
        }

        /// <summary>
        /// Helper function <c>switchMode</c> will switch to the provided mode, toggling the relevant tabs over.
        /// </summary>
        private void switchMode(string str)
        {
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


        // ----------------------  EXECUTION FUNCTIONS ----------------------------

        /// <summary>
        /// Function <c>executePlot</c> will execute a standard operation.
        /// </summary>
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
                handleInterpreterExceptions(exception);
            }
        }

        /// <summary>
        /// Function <c>executePlot</c> will execute a plotting operation.
        /// </summary>
        private void executePlot() {
            var constInput = tbConstInput.Text;
            var coefInput = tbCoefInput.Text;
            var xpoints = new List<double>();
            var ypoints = new List<double>();

            try {
                // will calculate j for given i

                // plotting points are between -100 and 100, which are the boundries for the plotting window
                for (int i = -100; i < 100; i++) {
                    // run graphing function for j
                    var j = lang.graphingFunction(coefInput, constInput, i.ToString(), ListModule.OfSeq(clist));
                    if (Xfirst) {
                        xpoints.Add(i);
                        ypoints.Add(j);
                    }
                    else {
                        xpoints.Add(j);
                        ypoints.Add(i);
                    }
                }

                // print plot
                WpfPlot1.Plot.AddScatter(xpoints.ToArray(), ypoints.ToArray());
                WpfPlot1.Refresh();

            }
            catch (Exception exception) {
                handleInterpreterExceptions(exception);
            }
        }



        // EVENT HANDLERS

        /// <summary>
        /// Event <c>Execute_Button_Click</c> will execute a standard operation.
        /// </summary>
        private void Execute_Button_Click(object sender, RoutedEventArgs e) {
                executeStandard();
                setDgSymList();
        }

        /// <summary>
        /// Event <c>Clear_Button_Click</c> clears the input, output and debug console, as well as resetting the plotting display.
        /// </summary>
        private void Clear_Button_Click(object sender, RoutedEventArgs e) {
            tbInput.Text = "";
            tbOutput.Text = "";
            tbErrOutput.Text = "...\n";

            resetPlot();
        }

        /// <summary>
        /// Event <c>Quit_Button_Click</c> shuts down the application.
        /// </summary>
        private void Quit_Button_Click(object sender, RoutedEventArgs e)
        {
            Application.Current.Shutdown();
        }

        /// <summary>
        /// Event <c>Execute_Reset_Symtable_Click</c> will clear the symbol table.
        /// </summary>
        private void Execute_Reset_Symtable_Click(object sender, RoutedEventArgs e)
        {
            this.clist = new List<Tuple<string, lang.NumberType>>();
            setDgSymList();
        }

        /// <summary>
        /// Event <c>ToggleHelpMessage</c> displays a popup box with the help message, containing the BNF.
        /// </summary>
        private void ToggleHelpMessage(object sender, RoutedEventArgs e)
        {
            MessageBox.Show("This is the help text for our application. Here you will find the BNF and " +
                "valid symbols to use in the query.\n\nNumber Types:\nThere are 3 number types in this application " +
                "- integers, floats and rationals. Integers are represented by whole numbers, floating points represented" +
                " by a number with a decimal place, and finally rational numbers uing the backslash in a fraction (i.e. 2 / 3" +
                " is simply 2 divided by 3 as integers, but 2\\3 represents 2 thirds as a rational number.)\n\n" + BNF, 
                "Help Text", MessageBoxButton.OK);
        }

        /// <summary>
        /// Event <c>ToggleHelpMessage</c> displays a popup box with the help message, containing the BNF.
        /// </summary>
        private void tbInput_TextChanged(object sender, TextChangedEventArgs e)
         {
            tbInputPlaceholder.Visibility = tbInput.Text != "" ? Visibility.Hidden : Visibility.Visible;
        }


        /// <summary>
        /// Event <c>TabControl_Output_SelectionChanged</c> changes the mode to the selected one on the output tab change.
        /// </summary>
        private void TabControl_Output_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            var header = ((TabItem)tcOutputTabs.SelectedItem).Header;
            string headerString = header.ToString() ?? "Standard";
            if (header != null)
            {
                switchMode(str: headerString);
            }
        }

        /// <summary>
        /// Event <c>TabControl_Input_SelectionChanged</c> changes the mode to the selected one on the input tab change.
        /// </summary>
        private void TabControl_Input_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            var header = ((TabItem)tcInputTabs.SelectedItem).Header;
            string headerString = header.ToString() ?? "Standard";
            if (header != null)
            {
                switchMode(str: headerString);
            }
        }

        /// <summary>
        /// Event <c>Button_Switch_Xy_Click</c> switches the X and Y around in the linear function input area.
        /// </summary>
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

        /// <summary>
        /// Event <c>Button_Execute_Plot_Click</c> executes a plotting operation.
        /// </summary>
        private void Button_Execute_Plot_Click(object sender, RoutedEventArgs e)
        {
            executePlot();
            setDgSymList();
        }

        /// <summary>
        /// Event <c>Execute_Run_Tests_Click</c> runs the predefined F# unit tests, and displays the result in a popup.
        /// </summary>
        private void Execute_Run_Tests_Click(object sender, RoutedEventArgs e)
        {
            ConsoleWriter consoleWriter = new ConsoleWriter(tbErrOutput);
            Console.SetOut(consoleWriter);

            // test result
            var res = lang.runTests;

            // counts
            int passcount = 0;
            int totalcount = 0;

            // lists of outputs
            List<string> failureMsgs = new List<string>();
            List<string> allMsgs = new List<string>();

            foreach (var item in res)
            {
                // if the pass/fail bool is true
                if (item.Item2)
                {
                    passcount++;
                }
                else {
                    failureMsgs.Add(item.Item1);
                }

                // increment count and add message to list
                totalcount++;
                allMsgs.Add(item.Item1);
            }

            // if there are failures
            if (passcount < totalcount)
            {
                // create a string of all failure messages
                string msg = "";
                foreach (var item in failureMsgs)
                {
                    msg += item + "\n";
                }

                // popup output
                MessageBox.Show("Not all tests passed.\n" + msg, "Test Results: " + passcount + "/" + totalcount + " passed", MessageBoxButton.OK, MessageBoxImage.Warning);
            }
            else {

                // create condensed list of all pass messages
                string msg = "";
                foreach (var item in allMsgs)
                {
                    msg += "  (" + item + ")  ";
                }

                // popup output
                MessageBox.Show("All tests passed. (" + passcount + "/" + totalcount + ")\n\n Raw Test Output:\n" + msg, "Test Results: " + passcount + "/" + totalcount + " passed", MessageBoxButton.OK, MessageBoxImage.Asterisk);
            }


        }
    }
}
