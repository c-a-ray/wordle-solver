/**
 * API Service for communicating with the Common Lisp backend
 */

// Default suggestions to use when the server doesn't have any yet
export const DEFAULT_SUGGESTIONS = [
  "WORDS",
  "WORLD",
  "WORTH",
  "WOULD",
  "WOUND",
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
    console.error("Error resetting game:", error);
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
  try {
    const response = await fetch(`/api/guess`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        word,
        colors,
        guess_number: guessNumber,
      }),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(
        errorData.error || `Server responded with ${response.status}`,
      );
    }

    const result = await response.json();
    return Array.isArray(result) ? result : DEFAULT_SUGGESTIONS;
  } catch (error) {
    console.error("Error submitting guess:", error);
    // Return default suggestions on error
    return DEFAULT_SUGGESTIONS;
  }
};

/**
 * Get suggestions based on current game state
 * @returns {Promise<Array>} Array of suggested words
 */
export const getSuggestions = async () => {
  try {
    const response = await fetch(`/api/suggest`, {
      method: "GET",
      headers: {
        "Content-Type": "application/json",
      },
    });

    if (!response.ok) {
      throw new Error(`Server responded with ${response.status}`);
    }

    const result = await response.json();

    // Check if we got a proper array of suggestions or an object with status
    if (Array.isArray(result)) {
      return result;
    } else if (result.status === "No suggestions available") {
      // Server says no suggestions, return default ones
      return DEFAULT_SUGGESTIONS;
    } else {
      return DEFAULT_SUGGESTIONS;
    }
  } catch (error) {
    console.error("Error getting suggestions:", error);
    // Return default suggestions on error
    return DEFAULT_SUGGESTIONS;
  }
};
