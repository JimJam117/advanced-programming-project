﻿using System;
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

        public MainWindow()
        {
            InitializeComponent();
            tbErrOutput.Text = "...\n";
            clist = new List<Tuple<string, int>>();
        }

        private void Execute_Button_Click(object sender, RoutedEventArgs e)
        {
            ConsoleWriter consoleWriter = new ConsoleWriter(tbErrOutput);
            Console.SetOut(consoleWriter);

            var text = tbInput.Text;
            
            try
            {
                Tuple<string, FSharpList<Tuple<string, int>>> output = lang.main_wpf(text, true, ListModule.OfSeq(clist), "");
                var outputStr = output.Item1; // output string
                flist = output.Item2; // output sym list in F# syntax
                var ulist = addSymLists(clist, flist.ToList()); // union of current clist and new flist
                clist = ulist.ToList(); // setting clist to union list

                // if there is an output string, display. Else clear output field
                if (outputStr != null) { tbOutput.Text = outputStr.ToString(); }
                else { tbOutput.Text = ""; }

                Console.WriteLine("SYMLIST: " + ListModule.OfSeq(clist));
                
            } catch(Exception exception) {
                if (exception.Message == "Lexer error") {
                    Console.WriteLine("Lexer error! Please make sure you have the correct symbols in the query.");
                }
                else if (exception.Message == "Parser error") {
                    Console.WriteLine("Parser error! Please make sure you have the correct symbols in the query.");
                }
                else {
                    Console.WriteLine(exception.Message);
                }
            }


            //tbErrOutput.Text = "Syntax Error!";

            //double[] dataX = new double[] { 1, 2, 3, 4, 5 };
            //double[] dataY = new double[] { 1, 4, 9, 16, 25 };

            //WpfPlot1.Plot.AddScatter(dataX, dataY);
            //WpfPlot1.Plot.AddLine(1,1,5,5);
            //WpfPlot1.Refresh();


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


        private void Clear_Button_Click(object sender, RoutedEventArgs e)
        {
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
    }
}
