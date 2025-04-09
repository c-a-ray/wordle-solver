import React from "react";
import "./ErrorMessage.css";

/**
 * Error message component
 * @param {Object} props - Component props
 * @param {string} props.message - Error message to display
 * @param {boolean} props.isSuccess - Whether this is a success message
 * @returns {JSX.Element} Rendered error message component
 */
const ErrorMessage = ({ message, isSuccess = false }) => {
  const className = isSuccess ? "success-message" : "error-message";
  return <div className={className}>{message}</div>;
};

export default ErrorMessage;
