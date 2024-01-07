using System.IO;
using System.Text;
using System.Windows.Controls;

namespace Group_9_Project
{
    internal class ConsoleWriter : TextWriter
    {
        // textbox to work with
        public TextBox ConsoleTextBox;

        // ctor
        public ConsoleWriter(TextBox tb)
        {
            ConsoleTextBox = tb;
        }

        // write function. Simply add the input text to the tb text content
        public override void Write(string input)
        {
            ConsoleTextBox.Text += input;
        }

        public override Encoding Encoding => Encoding.Unicode;
    }
}
