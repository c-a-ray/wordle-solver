import React from "react";
import "./Loading.css";

/**
 * Loading spinner component
 * @returns {JSX.Element} Rendered loading spinner
 */
const Loading = () => {
  return (
    <div className="loading-overlay">
      <div className="loading-spinner">
        <div className="spinner"></div>
        <p>Loading...</p>
      </div>
    </div>
  );
};

export default Loading;
