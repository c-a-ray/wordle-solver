import React from "react";

const Instructions = () => {
  return (
    <div className="text-gray-300 text-center max-w-lg mx-auto">
      <h3 className="text-blue-400 text-lg font-bold mb-3 text-center">Color Guide</h3>
      <div className="flex justify-center gap-4">
        <div className="text-center">
          <span className="inline-block px-2 py-1 bg-green-600 text-white text-xs rounded-md mb-1">
            Green
          </span>
          <p className="text-sm">correct position</p>
        </div>
        <div className="text-center">
          <span className="inline-block px-2 py-1 bg-yellow-500 text-white text-xs rounded-md mb-1">
            Yellow
          </span>
          <p className="text-sm">wrong position</p>
        </div>
        <div className="text-center">
          <span className="inline-block px-2 py-1 bg-gray-600 text-white text-xs rounded-md mb-1">
            Gray
          </span>
          <p className="text-sm">not in word</p>
        </div>
      </div>
    </div>
  );
};

export default Instructions;
