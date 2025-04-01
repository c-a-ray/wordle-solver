import React from "react";

const Sidebar = ({ suggestions }) => {
  return (
    <div className="w-80 h-screen fixed right-0 top-0 bg-gray-900 border-l border-gray-700 flex flex-col overflow-hidden shadow-lg">
      <div className="p-5 border-b border-gray-800"></div>

      <div className="overflow-y-auto flex-1 p-4">
        {suggestions && suggestions.length > 0 ? (
          <div className="space-y-3">
            {suggestions.map((word, index) => (
              <div
                key={index}
                className="bg-gray-800 hover:bg-gray-700 px-4 py-3 rounded-xl font-medium uppercase text-white text-center transition-all duration-200 transform hover:scale-105 hover:shadow-lg hover:shadow-blue-900/20 cursor-pointer"
              >
                {word}
              </div>
            ))}
          </div>
        ) : (
          <p className="text-gray-400 italic text-center mt-4">
            Enter a word and color patterns to get suggestions
          </p>
        )}
      </div>

      <div className="p-4 text-xs text-gray-500 text-center border-t border-gray-800">
        Complete a row to see suggestions
      </div>
    </div>
  );
};

export default Sidebar;
