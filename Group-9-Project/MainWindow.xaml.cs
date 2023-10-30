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

namespace Group_9_Project
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Execute_Button_Click(object sender, RoutedEventArgs e)
        {
            var text = tbInput.Text;
            tbOutput.Text = text;
            tbErrOutput.Text = "Syntax Error!";


            double[] dataX = new double[] { 1, 2, 3, 4, 5 };
            double[] dataY = new double[] { 1, 4, 9, 16, 25 };

            WpfPlot1.Plot.AddScatter(dataX, dataY);
            WpfPlot1.Refresh();


        }


        private void Clear_Button_Click(object sender, RoutedEventArgs e)
        {
            tbInput.Text = "";
            tbOutput.Text = "";
            tbErrOutput.Text = "...";

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

    }
}
