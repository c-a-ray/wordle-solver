import React from "react";

const Sidebar = ({ suggestions }) => {
  return (
    <div
      style={{
        width: "320px",
        height: "100vh",
        backgroundColor: "#0f172a",
        borderRight: "1px solid #334155",
      }}
      className="overflow-y-auto"
    >
      <div className="p-6 border-b border-gray-800 text-2xl font-bold uppercase tracking-wide text-white">
        Suggestions
      </div>
      <div className="p-4 space-y-4">
        {suggestions.map((word, index) => (
          <div
            key={index}
            className="bg-gray-800 hover:bg-gray-700 text-lg font-semibold uppercase text-center px-4 py-3 rounded-xl shadow-md transition duration-200 transform hover:scale-105 text-white"
          >
            {word}
          </div>
        ))}
      </div>
    </div>
  );
};
export default Sidebar;
