import React from "react";
import "./ErrorMessage.css";

/**
 * Error message component
 * @param {Object} props - Component props
 * @param {string} props.message - Error message to display
 * @returns {JSX.Element} Rendered error message component
 */
const ErrorMessage = ({ message }) => {
  return <div className="error-message">{message}</div>;
};

export default ErrorMessage;
