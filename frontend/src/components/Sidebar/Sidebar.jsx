import React from "react";
import "./Sidebar.css";

/**
 * Sidebar component that displays word suggestions
 * @param {Object} props - Component props
 * @param {Array<string>} props.suggestions - Array of word suggestions
 * @returns {JSX.Element} Rendered sidebar component
 */
const Sidebar = ({ suggestions }) => {
  return (
    <div className="sidebar">
      <div className="sidebar-header">Suggestions</div>
      <div className="suggestions-container">
        {suggestions.map((word, index) => (
          <div key={index} className="suggestion-item">
            {word}
          </div>
        ))}
      </div>
    </div>
  );
};

export default Sidebar;
