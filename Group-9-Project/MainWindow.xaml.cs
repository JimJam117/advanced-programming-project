using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using FSharpCodeLib;
using Microsoft.FSharp.Collections;
using ScottPlot;
using static Microsoft.FSharp.Core.ByRefKinds;

namespace Group_9_Project
{
    
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public Microsoft.FSharp.Collections.FSharpList<Tuple<string, int>> flist;
        public List<Tuple<string, int>> clist;
        public string BNF = "// Grammar in (E)BNF:\r\n<VA> ::= <varID> \"=\" <E>\r\n<E> ::= <T> <Eopt>\r\n<Eopt> ::= \"+\" <T> <Eopt> | \"-\" <T> <Eopt> | <empty>\r\n<T> ::= <NR> <Topt>\r\n<Topt> ::= \"*\" <NR> <Topt> | \"/\" <NR> <Topt> | <empty>\r\n<NR> ::= [\"Num\" | \"varVal\" ] <value> | \"(\" <E> \")\"\r\n<varID> ::= [a-z,A-Z]+  (* varVal is fetched from symbol table/list with key varID *)";

        public enum CurrentMode {
            STANDARD = 1,
            PLOTTING = 2
         }

        public CurrentMode currentMode = CurrentMode.STANDARD;

        public MainWindow()
        {
            InitializeComponent();
            tbErrOutput.Text = "...\n";
            clist = new List<Tuple<string, int>>();
            tbBNF.Text = BNF;
        }

        private void Execute_Reset_Symtable_Click(object sender, RoutedEventArgs e)
        {
            this.clist = new List<Tuple<string, int>>();
            dgSymlist.ItemsSource = clist;
        }

        private void executeStandard() {
            ConsoleWriter consoleWriter = new ConsoleWriter(tbErrOutput);
            Console.SetOut(consoleWriter);

            var text = tbInput.Text;

            try {
                Tuple<string, FSharpList<Tuple<string, int>>> output = lang.main_wpf(text, true, ListModule.OfSeq(clist), "");
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

        private void executePlotting() {
            ConsoleWriter consoleWriter = new ConsoleWriter(tbErrOutput);
            Console.SetOut(consoleWriter);

            var text = tbInput.Text;
            try {
                List<Tuple<string, int>> plotCList = new List<Tuple<string, int>>();

                Tuple<string, FSharpList<Tuple<string, int>>> output = lang.main_wpf(text, true, ListModule.OfSeq(plotCList), "");
                var outputStr = output.Item1; // output string
                flist = output.Item2; // output sym list in F# syntax
                var ulist = addSymLists(plotCList, flist.ToList()); // union of current clist and new flist
                plotCList = ulist.ToList(); // setting clist to union list

                // update symlist
                Console.WriteLine("PLOT SYMLIST: " + ListModule.OfSeq(plotCList));


                if (plotCList.Count() >= 2) {
                    Console.WriteLine("DEBUG - PLOTTING!");
                    double[] dataX = new double[200];
                    double[] dataY = new double[200];
                    int i = 0;
                    for (float x = -10f; x < 10f; x += 0.1f) {
                        dataX[i] = x * plotCList.First().Item2;
                        dataY[i] = x * plotCList.Last().Item2; // change this
                        i++;
                    }

                    WpfPlot1.Plot.AddScatter(dataX, dataY);
                    WpfPlot1.Refresh();
                }
                else {
                    Console.WriteLine("Error - invalid polynomial");
                }
            }
            catch (Exception exception) {
                Console.WriteLine("Polynomial Error:" + exception.Message);

            }
        }

        private void Execute_Button_Click(object sender, RoutedEventArgs e) {
            switch (currentMode)
            {
                case CurrentMode.STANDARD:
                    executeStandard();
                    break;
                case CurrentMode.PLOTTING:
                    executePlotting();
                    break;
                default:
                    ConsoleWriter consoleWriter = new ConsoleWriter(tbErrOutput);
                    Console.SetOut(consoleWriter);
                    Console.WriteLine("ERROR: MODE INVALID!");
                    break;
            }

            dgSymlist.ItemsSource = clist;
        }

        private List<Tuple<string, int>> addSymLists(List<Tuple<string, int>> inputList1, List<Tuple<string, int>> inputList2) {
            List<Tuple<string, int>> newList = inputList1;
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

            WpfPlot1.Plot.Clear();
            WpfPlot1.Refresh();
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

        private void TabControl_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            switch ((tcTabs.SelectedItem as TabItem).Header.ToString())
            {
                case "Standard":
                    currentMode = CurrentMode.STANDARD; break;
                case "Plotting":
                    currentMode = CurrentMode.PLOTTING; break;

                default:
                    break;
            }
            
        }
    }
}
