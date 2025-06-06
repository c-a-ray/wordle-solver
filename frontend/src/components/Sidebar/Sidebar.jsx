import React from "react";
import "./Sidebar.css";

/**
 * Sidebar component that displays word suggestions
 * @param {Object} props - Component props
 * @param {Array<string>|null} props.suggestions - Array of word suggestions or null if no matches
 * @param {Function} props.onSuggestionClick - Callback function when a suggestion is clicked
 * @returns {JSX.Element} Rendered sidebar component
 */
const Sidebar = ({ suggestions, onSuggestionClick }) => {
  return (
    <div className="sidebar">
      <div className="sidebar-header">Suggestions</div>
      <div className="suggestions-container">
        {suggestions === null || suggestions.length == 0 ? (
          <div className="no-matches">
            <p>No words match this pattern.</p>
            <p>Are you sure you entered the Wordle feedback correctly?</p>
          </div>
        ) : (
          suggestions.map((word, index) => (
            <div 
              key={index} 
              className="suggestion-item"
              onClick={() => onSuggestionClick(word)}
            >
              {word}
            </div>
          ))
        )}
      </div>
    </div>
  );
};

export default Sidebar;
