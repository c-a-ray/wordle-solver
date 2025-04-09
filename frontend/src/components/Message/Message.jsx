import React from "react";
import "./Message.css";

/**
 * Message component
 * @param {Object} props - Component props
 * @param {string} props.message - Message to display
 * @param {boolean} props.isSuccess - Whether this is a success message
 * @returns {JSX.Element} Rendered message component
 */
const Message = ({ message, isSuccess = false }) => {
  const className = isSuccess ? "success-message" : "error-message";
  return <div className={className}>{message}</div>;
};

export default Message;
