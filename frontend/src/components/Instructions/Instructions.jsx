import React from "react";
import ErrorMessage from "../ErrorMessage";
import "./Instructions.css";

/**
 * Instructions component for game instructions and error messages
 * @param {Object} props - Component props
 * @param {string} props.errorMessage - Error message to display
 * @returns {JSX.Element} Rendered instructions component
 */
const Instructions = ({ errorMessage }) => {
  return (
    <div className="instructions-container">
      <p className="instruction-text mb-2">
        Click a cell and type to enter a word. Use ↑/↓ to change colors.
      </p>
      <p className="instruction-text">
        Press Enter when your word is complete to get suggestions.
      </p>
      {errorMessage && <ErrorMessage message={errorMessage} />}
    </div>
  );
};

export default Instructions;
