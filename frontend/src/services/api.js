export const DEFAULT_SUGGESTIONS = [
  "ROATE",
  "SLATE",
  "CRANE",
  "TRACE",
  "ADIEU",
];

/**
 * Convert cell colors from our frontend format to the backend format
 * @param {Array} grid - The current grid state
 * @param {number} rowIndex - The row to convert
 * @returns {Array} Array of color strings for the backend
 */
export const convertColorsForBackend = (grid, rowIndex) => {
  const colorMap = {
    gray: "gray",
    yellow: "yellow",
    green: "green",
    empty: "gray", // Default empty to gray for backend
  };

  return grid[rowIndex].map((cell) => colorMap[cell.color]);
};

/**
 * Reset the game on the server
 * @returns {Promise<Object>} The response from the server
 */
export const resetGame = async () => {
  try {
    const response = await fetch(`/api/reset`, {
      method: "GET",
      headers: {
        "Content-Type": "application/json",
      },
    });

    if (!response.ok) {
      throw new Error(`Server responded with ${response.status}`);
    }

    return await response.json();
  } catch (error) {
    throw error;
  }
};

/**
 * Submit a guess to the server
 * @param {string} word - The guessed word
 * @param {Array} colors - The colors for each letter
 * @param {number} guessNumber - The current guess number
 * @returns {Promise<Array>} Array of suggested words
 */
export const submitGuess = async (word, colors, guessNumber) => {
  const payload = {
    word: word.toLowerCase(),
    colors: colors,
    "guess-number": guessNumber,
  };

  const response = await fetch(`/api/guess`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    credentials: "include", // Include cookies for session
    body: JSON.stringify(payload),
  });

  if (!response.ok) {
    let errorMessage;
    try {
      const errorData = await response.json();
      errorMessage =
        errorData.error || `Server responded with ${response.status}`;
    } catch (e) {
      errorMessage = `Server responded with ${response.status}`;
    }
    throw new Error(errorMessage);
  }

  const result = await response.json();
  if (result == null || (Array.isArray(result) && result.length == 0)) {
    return null;
  }
  return result;
};
