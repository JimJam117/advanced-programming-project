using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Controls;

namespace Group_9_Project
{
    internal class ConsoleWriter : TextWriter
    {
        public TextBox ConsoleTextBox;

        public ConsoleWriter(TextBox tb)
        {
            ConsoleTextBox = tb;
        }
        public override void Write(string input)
        {
            ConsoleTextBox.Text += input;
        }

        public override Encoding Encoding => Encoding.Unicode;
    }
}
