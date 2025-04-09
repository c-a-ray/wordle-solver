import React from "react";
import "./ResetButton.css";

/**
 * Reset button component
 * @param {Object} props - Component props
 * @param {function} props.onClick - Click handler function
 * @param {boolean} props.disabled - Whether the button is disabled
 * @returns {JSX.Element} Rendered reset button
 */
const ResetButton = ({ onClick, disabled }) => {
  return (
    <button className="reset-button" onClick={onClick} disabled={disabled}>
      Reset Game
    </button>
  );
};

export default ResetButton;
