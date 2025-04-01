import React from "react";

const WordleCell = ({
  letter,
  color,
  isActive,
  onKeyDown,
  onClick,
  cellRef,
}) => {
  console.log("Rendering WordleCell:", { letter, color, isActive });
  
  // Simplify the color logic to debug
  const getColorClasses = () => {
    switch (color) {
      case "gray":
        return "bg-gray-500 text-white border-gray-500";
      case "yellow":
        return "bg-yellow-500 text-white border-yellow-500";
      case "green":
        return "bg-green-500 text-white border-green-500";
      default:
        return "bg-white dark:bg-gray-800 border-gray-300 dark:border-gray-600";
    }
  };

  return (
    <div
      ref={cellRef}
      className={`
        w-14 h-14 
        border-2
        flex justify-center items-center
        text-2xl font-bold uppercase
        cursor-pointer
        ${getColorClasses()}
        ${isActive ? "border-gray-500 dark:border-gray-400" : ""}
      `}
      onClick={onClick}
      onKeyDown={onKeyDown}
      tabIndex={isActive ? 0 : -1}
    >
      {letter || "_"}
    </div>
  );
};

export default WordleCell;
