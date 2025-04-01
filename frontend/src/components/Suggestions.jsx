import React from "react";

const Suggestions = ({ suggestions }) => {
  if (!suggestions || suggestions.length === 0) {
    return null;
  }

  return (
    <div className="w-full mt-6 text-center">
      <h2 className="text-xl font-semibold mb-3 text-gray-800 dark:text-gray-200">
        Suggested Words
      </h2>
      <ul className="flex flex-wrap gap-2 justify-center">
        {suggestions.slice(0, 10).map((word, index) => (
          <li
            key={index}
            className="bg-gray-100 dark:bg-gray-700 px-3 py-2 rounded font-medium uppercase text-gray-800 dark:text-gray-200"
          >
            {word}
          </li>
        ))}
      </ul>
    </div>
  );
};

export default Suggestions;
