import React from "react";

const Instructions = () => {
  return (
    <div className="text-center mb-8 text-gray-600 dark:text-gray-300 w-full flex flex-col items-center">
      <p className="mb-3 text-center w-full">
        Enter your guess and use ↑/↓ arrows to change cell colors.
      </p>
      <div className="text-center w-full flex justify-center flex-wrap">
        <div className="mx-1">
          <span className="inline-block px-2 py-1 mr-1 bg-green-500 text-white text-xs rounded">
            Green
          </span>{" "}
          = correct position
        </div>
        <div className="mx-1">
          <span className="inline-block px-2 py-1 mr-1 bg-yellow-500 text-white text-xs rounded">
            Yellow
          </span>{" "}
          = wrong position
        </div>
        <div className="mx-1">
          <span className="inline-block px-2 py-1 mr-1 bg-gray-500 text-white text-xs rounded">
            Gray
          </span>{" "}
          = not in word
        </div>
      </div>
    </div>
  );
};

export default Instructions;
