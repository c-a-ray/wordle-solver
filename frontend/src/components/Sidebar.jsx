import React from "react";
import "./Sidebar.css";

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
