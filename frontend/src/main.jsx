import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App";
import "./index.css";

console.log("main.jsx is executing");
const rootElement = document.getElementById("root");
console.log("Root element:", rootElement);

if (rootElement) {
  try {
    const root = ReactDOM.createRoot(rootElement);
    root.render(
      <React.StrictMode>
        <App />
      </React.StrictMode>
    );
    console.log("App rendered successfully");
  } catch (error) {
    console.error("Error rendering app:", error);
    document.body.innerHTML = '<div style="padding: 20px; text-align: center;"><h1>Error</h1><p>Error loading application. Check console for details.</p></div>';
  }
} else {
  console.error("Root element not found");
  document.body.innerHTML = '<div style="padding: 20px; text-align: center;"><h1>Error</h1><p>Root element not found.</p></div>';
}