"""
Ethical AI Advisor - GUI Chatbot with Prolog Integration
Terminal-style interface that connects directly to your Prolog system
"""

import tkinter as tk
from tkinter import scrolledtext, font
from datetime import datetime
import subprocess
import threading
import os

class PrologChatbotGUI:
    def __init__(self, root):
        self.root = root
        self.root.title("Ethical AI Advisor - Prolog Chatbot")
        self.root.geometry("1000x700")
        self.root.configure(bg="#1e1e1e")
        
        # Prolog paths
        self.SWIPL_EXE = "D:/download/swipl/bin/swipl.exe"
        self.MAIN_PL = "C:/Users/User/OneDrive/Desktop/Prolog/main.pl"
        
        # Prolog process
        self.prolog_process = None
        self.waiting_for_input = False
        self.current_prompt = ""
        
        # Create GUI
        self.create_header()
        self.create_chat_area()
        self.create_input_area()
        self.create_control_buttons()
        
        # Start Prolog
        self.start_prolog()
    
    def create_header(self):
        """Create header"""
        header_frame = tk.Frame(self.root, bg="#0d47a1", height=70)
        header_frame.pack(fill=tk.X)
        header_frame.pack_propagate(False)
        
        title = tk.Label(
            header_frame,
            text="Ethical AI Advisor - Prolog Chatbot",
            font=("Arial", 18, "bold"),
            bg="#0d47a1",
            fg="white"
        )
        title.pack(pady=15)
        
        # Status label
        self.status_label = tk.Label(
            header_frame,
            text="Starting Prolog...",
            font=("Arial", 9),
            bg="#0d47a1",
            fg="#90caf9"
        )
        self.status_label.place(relx=1.0, rely=0.5, anchor='e', x=-20)
    
    def create_chat_area(self):
        """Create terminal-style chat area"""
        chat_frame = tk.Frame(self.root, bg="#1e1e1e")
        chat_frame.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        
        # Create scrolled text widget
        self.chat_display = scrolledtext.ScrolledText(
            chat_frame,
            wrap=tk.WORD,
            font=("Consolas", 11),
            bg="#2b2b2b",
            fg="#f0f0f0",
            insertbackground="white",
            state=tk.DISABLED,
            spacing3=3
        )
        self.chat_display.pack(fill=tk.BOTH, expand=True)
        
        # Configure tags for styling
        self.chat_display.tag_config("prolog_output", foreground="#90caf9")
        self.chat_display.tag_config("user_input", foreground="#4caf50", font=("Consolas", 11, "bold"))
        self.chat_display.tag_config("prompt", foreground="#ffa726")
        self.chat_display.tag_config("error", foreground="#f44336")
        self.chat_display.tag_config("success", foreground="#4caf50")
    
    def create_input_area(self):
        """Create input area"""
        input_frame = tk.Frame(self.root, bg="#1e1e1e")
        input_frame.pack(fill=tk.X, padx=10, pady=(0, 10))
        
        # Prompt label
        self.prompt_display = tk.Label(
            input_frame,
            text=">>>",
            font=("Consolas", 11, "bold"),
            bg="#1e1e1e",
            fg="#ffa726"
        )
        self.prompt_display.pack(side=tk.LEFT, padx=(0, 5))
        
        # Input field
        self.input_field = tk.Entry(
            input_frame,
            font=("Consolas", 11),
            bg="#2b2b2b",
            fg="white",
            insertbackground="white",
            relief=tk.FLAT,
            borderwidth=5
        )
        self.input_field.pack(side=tk.LEFT, fill=tk.X, expand=True, ipady=5)
        self.input_field.bind("<Return>", lambda e: self.send_to_prolog())
        self.input_field.focus()
        
        # Send button
        send_btn = tk.Button(
            input_frame,
            text="Send",
            command=self.send_to_prolog,
            font=("Arial", 11, "bold"),
            bg="#0d47a1",
            fg="white",
            relief=tk.FLAT,
            padx=20,
            cursor="hand2"
        )
        send_btn.pack(side=tk.RIGHT, padx=(10, 0))
    
    def create_control_buttons(self):
        """Create control buttons"""
        button_frame = tk.Frame(self.root, bg="#1e1e1e")
        button_frame.pack(fill=tk.X, padx=10, pady=(0, 10))
        
        buttons = [
            ("Clear Screen", self.clear_screen),
            ("Restart Prolog", self.restart_prolog),
            ("Exit", self.exit_app)
        ]
        
        for text, command in buttons:
            btn = tk.Button(
                button_frame,
                text=text,
                command=command,
                font=("Arial", 9),
                bg="#424242",
                fg="white",
                relief=tk.FLAT,
                padx=15,
                pady=5,
                cursor="hand2"
            )
            btn.pack(side=tk.LEFT, padx=5)
    
    def add_text(self, text, tag="prolog_output"):
        """Add text to chat display"""
        self.chat_display.config(state=tk.NORMAL)
        self.chat_display.insert(tk.END, text, tag)
        self.chat_display.see(tk.END)
        self.chat_display.config(state=tk.DISABLED)
    
    def start_prolog(self):
        """Start Prolog process"""
        if not os.path.exists(self.SWIPL_EXE):
            self.add_text("❌ Error: SWI-Prolog not found!\n", "error")
            self.add_text(f"Expected at: {self.SWIPL_EXE}\n\n", "error")
            self.status_label.config(text="Prolog not found", fg="#f44336")
            return
        
        if not os.path.exists(self.MAIN_PL):
            self.add_text("❌ Error: main.pl not found!\n", "error")
            self.add_text(f"Expected at: {self.MAIN_PL}\n\n", "error")
            self.status_label.config(text="main.pl not found", fg="#f44336")
            return
        
        try:
            # Start Prolog process
            self.prolog_process = subprocess.Popen(
                [self.SWIPL_EXE, "-s", self.MAIN_PL],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                bufsize=1,
                universal_newlines=True
            )
            
            self.status_label.config(text="✓ Connected", fg="#4caf50")
            
            # Start reading output
            threading.Thread(target=self.read_prolog_output, daemon=True).start()
            
        except Exception as e:
            self.add_text(f"❌ Error starting Prolog: {e}\n\n", "error")
            self.status_label.config(text="Connection failed", fg="#f44336")
    
    def read_prolog_output(self):
        """Read Prolog output continuously"""
        buffer = ""
        
        while self.prolog_process and self.prolog_process.poll() is None:
            try:
                char = self.prolog_process.stdout.read(1)
                
                if char:
                    buffer += char
                    
                    # Check if it's a prompt
                    if buffer.endswith("Enter your choice (1-5): ") or \
                       buffer.endswith("Enter dilemma ID: ") or \
                       buffer.endswith("Enter framework name: ") or \
                       buffer.endswith("Enter dilemma ID to compare: ") or \
                       buffer.endswith("?- ") or \
                       buffer.endswith("|: "):
                        
                        self.root.after(0, lambda b=buffer: self.add_text(b, "prompt"))
                        self.waiting_for_input = True
                        self.current_prompt = buffer
                        buffer = ""
                    
                    # Display accumulated output
                    elif '\n' in buffer:
                        self.root.after(0, lambda b=buffer: self.add_text(b, "prolog_output"))
                        buffer = ""
            
            except Exception as e:
                print(f"Error reading output: {e}")
                break
    
    def send_to_prolog(self):
        """Send input to Prolog"""
        user_input = self.input_field.get().strip()
        
        if not user_input:
            return
        
        # Clear input field
        self.input_field.delete(0, tk.END)
        
        # Display user input
        self.add_text(f"{user_input}\n", "user_input")
        
        # Send to Prolog
        if self.prolog_process and self.prolog_process.poll() is None:
            try:
                # Add period if needed and not already present
                if not user_input.endswith('.'):
                    user_input += '.'
                
                self.prolog_process.stdin.write(user_input + '\n')
                self.prolog_process.stdin.flush()
                self.waiting_for_input = False
                
            except Exception as e:
                self.add_text(f"❌ Error sending to Prolog: {e}\n", "error")
        else:
            self.add_text("❌ Prolog process not running!\n", "error")
    
    def clear_screen(self):
        """Clear the chat display"""
        self.chat_display.config(state=tk.NORMAL)
        self.chat_display.delete(1.0, tk.END)
        self.chat_display.config(state=tk.DISABLED)
        self.add_text("Screen cleared.\n\n", "success")
    
    def restart_prolog(self):
        """Restart Prolog process"""
        if self.prolog_process:
            self.prolog_process.terminate()
            self.prolog_process.wait()
        
        self.clear_screen()
        self.add_text("Restarting Prolog...\n\n", "success")
        self.start_prolog()
    
    def exit_app(self):
        """Exit application"""
        if self.prolog_process:
            self.prolog_process.terminate()
            self.prolog_process.wait()
        self.root.quit()

# Run the application
if __name__ == "__main__":
    root = tk.Tk()
    app = PrologChatbotGUI(root)
    root.mainloop()