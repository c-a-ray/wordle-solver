import React from "react";
import ErrorMessage from "../ErrorMessage";
import "./Instructions.css";

/**
 * Instructions component for game instructions and error messages
 * @param {Object} props - Component props
 * @param {string} props.errorMessage - Error message to display
 * @param {boolean} props.isSuccess - Whether the message is a success message
 * @returns {JSX.Element} Rendered instructions component
 */
const Instructions = ({ errorMessage, isSuccess = false }) => {
  return (
    <div className="instructions-container">
      <p className="instruction-text mb-2">
        Click a cell and type to enter a word. Use ↑/↓ to change colors.
      </p>
      <p className="instruction-text mb-2">
        Press Enter when your word is complete to get suggestions.
      </p>
      <p className="instruction-text">
        Use Home/End keys to move to the first/last cell in a row.
      </p>
      {errorMessage && (
        <ErrorMessage message={errorMessage} isSuccess={isSuccess} />
      )}
    </div>
  );
};

export default Instructions;
