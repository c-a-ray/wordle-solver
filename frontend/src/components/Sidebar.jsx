import React from "react";

const Sidebar = ({ suggestions }) => {
  return (
    <div
      style={{
        width: "250px",
        height: "100vh",
        backgroundColor: "#15202b",
        borderRight: "1px solid #38444d",
        overflowY: "auto",
      }}
    >
      <div
        style={{
          padding: "16px",
          borderBottom: "1px solid #38444d",
          textAlign: "center",
          fontSize: "20px",
          fontWeight: "bold",
          textTransform: "uppercase",
          letterSpacing: "0.05em",
          color: "white",
        }}
      >
        Suggestions
      </div>
      <div
        style={{
          padding: "12px",
          display: "flex",
          flexDirection: "column",
          gap: "12px",
        }}
      >
        {suggestions.map((word, index) => (
          <div
            key={index}
            style={{
              backgroundColor: "#192734",
              color: "white",
              padding: "8px 12px",
              borderRadius: "8px",
              fontSize: "18px",
              fontWeight: "600",
              textAlign: "center",
              textTransform: "uppercase",
              boxShadow: "0 2px 4px rgba(0,0,0,0.2)",
              cursor: "pointer",
              transition: "all 0.2s",
            }}
            onMouseOver={(e) => {
              e.currentTarget.style.backgroundColor = "#22303c";
              e.currentTarget.style.transform = "scale(1.05)";
            }}
            onMouseOut={(e) => {
              e.currentTarget.style.backgroundColor = "#192734";
              e.currentTarget.style.transform = "scale(1)";
            }}
          >
            {word}
          </div>
        ))}
      </div>
    </div>
  );
};

export default Sidebar;
