###############################################################################
# Sudoku

####### plan of action#########
# *make grid*
# *fill grid with valid sudoku*
# remove values and leave enough to solve
# solve puzzle
# implement user interface
#
######################### sudokusolver########################################
rm(list = ls())

############################################################################
# build the sudoku solver
#
# Basic tools for solving sudoku  *may implement more advanced techniques later but lets just satrt with these for now*
# ns: Naked Single
# hs: Hidden Single
# np: Naked Pairs
# hp: Hidden Pairs
# ir: Intersection Removal
# xw: X-Wing
# yw: Y-Wing
#
#
# Naked Singles (NS): Naked Singles routine iterates through all the empty squares on the board, calculating the possible values in each cell based on Sudoku's rules. When we find a cell with a single possibility we fill in the Sudoku cell with that value. Then we remove that possibility from all the cells in the affected column, row and square regions. We run this routine first as all the ones below rely on the cell's update possibility lists.
#
# Hidden Singles (HS): Hidden Singles scan each row, column and 3 x 3 square for the "number of possibilities" (not the possibilities) for each number ranging from one to nine. When it finds a cell containing a possibility of a number that appears only once in a row, column or 3 x 3 square, we fill in the Sudoku board with that value. Then we remove that possibility from the affected column, row and 3 x 3 square regions.
#
# Naked Pairs (NP):
#
#   The first version of Naked Pairs searches each region (row, column and 3 x 3 square) for two possibility values that occur only twice in, and share two cells, which may or may not contain other possibilities. Because they occur
# only in these two cells, one of them must go in one cell and the other in the second. Therefore, any other values in these two cells may be eliminated.
#
# The second version scans each region (row, column and 3 x 3 square) for two cells, each containing ONLY the same two possibilities. Because one of these values must occur in each of the two cells, they cannot occur anywhere else in that region, and may be eliminated from the lists of possibilities for every other cell in the region.
#
# Hidden Pairs (HP):
#
#   Hidden Pairs are taken care of by the combination of Naked Singles and Hidden Singles algorithms.
#
# Intersection Removal (IR):
#
#   If exactly two or three possibilities exist for a single number in a 3 x 3 box and they are restricted to one row or column, their values may be eliminated from the rest of the row or column. The case of one possibility is covered by HS above.
#
# If exactly two or three possibilities exist for a single number in a row or column and it is bound by a 3 x 3 box, the number can be eliminated from the rest of the box. The case of one possibility is covered by HS above.
#
# X-Wing (XW):
#
#   Look for a single possibility value that occurs exactly twice in two rows. If the cells containing
# the value line up in two columns to form a rectangle, all occurrences of the value may be
# removed from both columns. This also works the other way, starting with columns and eliminating
# from rows.
#
# Y-Wing (YW)
# Y-wings work by eliminating possibilities from intersections of influence. Each cell exerts
# a range of influence on all the others cells in the same row, column, and box. Y-wing is a
# complex, advanced technique, so we present an example:
#
#   Suppose a cell has exactly two possibilities A and B. This cell AB is the pivot. Consider
# two cells, AC and BC, which are influenced by AB, but do not influence each other. If they
# each share exactly one possibility with AB and exactly one possibility with each other, then
# the possibility held in common between AC and BC, C, can be eliminated from every cell in
# the intersection of AC and BC's ranges of influence.
############################################################################################################

# if cells are filled in by row:
# floor(x/9)+1 gives the row that cell x is in and
# x%%9+1 gives column of cell x
rm(list = ls())

set.grid <- function(difficulty = "easy") {
	possibles <- vector(mode = "list", length = 81)
	row.possibles <- vector(mode = "list", length = 9)
	col.possibles <- vector(mode = "list", length = 9)
	box.possibles <- vector(mode = "list", length = 9)

	get.grid <- function() {
		sudoku.grid <- matrix(
			data = vector(mode = "integer", length = 9 * 9),
			nrow = 9,
			ncol = 9,
			byrow = TRUE,
			dimnames = list(letters[1:9], LETTERS[1:9])
		)

		while (sum(sudoku.grid) != 405) {
			sudoku.grid <- matrix(
				data = vector(mode = "integer", length = 9 * 9),
				nrow = 9,
				ncol = 9,
				byrow = TRUE,
				dimnames = list(letters[1:9], LETTERS[1:9])
			)

			box <- function(a, b) {
				if ((a >= 1 && a <= 3) && (b >= 1 && b <= 3)) {
					as.vector(b1)
				} else if ((a >= 1 && a <= 3) && (b >= 4 && b <= 6)) {
					as.vector(b2)
				} else if ((a >= 1 && a <= 3) && (b >= 7 && b <= 9)) {
					as.vector(b3)
				} else if ((a >= 4 && a <= 6) && (b >= 1 && b <= 3)) {
					as.vector(b4)
				} else if ((a >= 4 && a <= 6) && (b >= 4 && b <= 6)) {
					as.vector(b5)
				} else if ((a >= 4 && a <= 6) && (b >= 7 && b <= 9)) {
					as.vector(b6)
				} else if ((a >= 7 && a <= 9) && (b >= 1 && b <= 3)) {
					as.vector(b7)
				} else if ((a >= 7 && a <= 9) && (b >= 4 && b <= 6)) {
					as.vector(b8)
				} else if ((a >= 7 && a <= 9) && (b >= 7 && b <= 9)) {
					as.vector(b9)
				}
			}

			get.weights <- function(l) {
				if (l == 0 || l == 8 || l == 9) {
					seq(from = 1, to = 1, length.out = 9)
				} else {
					seq(from = 1 / (9 - l), to = 1 / (9 - l), length.out = 9)
				}
			}

			for (i in 1:9) {
				for (j in 1:9) {
					b1 <- sudoku.grid[1:3, 1:3]
					b2 <- sudoku.grid[1:3, 4:6]
					b3 <- sudoku.grid[1:3, 7:9]
					b4 <- sudoku.grid[4:6, 1:3]
					b5 <- sudoku.grid[4:6, 4:6]
					b6 <- sudoku.grid[4:6, 7:9]
					b7 <- sudoku.grid[7:9, 1:3]
					b8 <- sudoku.grid[7:9, 4:6]
					b9 <- sudoku.grid[7:9, 7:9]

					bad.values <- unique(c(
						as.vector(sudoku.grid[i, ]),
						as.vector(sudoku.grid[, j]),
						box(i, j)
					))
					bad.values <- bad.values[bad.values != 0]

					prob.weights <- get.weights(length(bad.values))
					prob.weights[bad.values] <- 0

					sudoku.grid[i, j] <- tryCatch(
						sample(x = 1:9, size = 1, prob = prob.weights),
						error = function(e) {
							0
						}
					)
				}
			}
		}
		return(sudoku.grid)
	}

	b2 <- function(a, b) {
		box1 <- current.grid[1:3, 1:3]
		box2 <- current.grid[1:3, 4:6]
		box3 <- current.grid[1:3, 7:9]
		box4 <- current.grid[4:6, 1:3]
		box5 <- current.grid[4:6, 4:6]
		box6 <- current.grid[4:6, 7:9]
		box7 <- current.grid[7:9, 1:3]
		box8 <- current.grid[7:9, 4:6]
		box9 <- current.grid[7:9, 7:9]
		if ((a >= 1 && a <= 3) && (b >= 1 && b <= 3)) {
			as.vector(box1)
		} else if ((a >= 1 && a <= 3) && (b >= 4 && b <= 6)) {
			as.vector(box2)
		} else if ((a >= 1 && a <= 3) && (b >= 7 && b <= 9)) {
			as.vector(box3)
		} else if ((a >= 4 && a <= 6) && (b >= 1 && b <= 3)) {
			as.vector(box4)
		} else if ((a >= 4 && a <= 6) && (b >= 4 && b <= 6)) {
			as.vector(box5)
		} else if ((a >= 4 && a <= 6) && (b >= 7 && b <= 9)) {
			as.vector(box6)
		} else if ((a >= 7 && a <= 9) && (b >= 1 && b <= 3)) {
			as.vector(box7)
		} else if ((a >= 7 && a <= 9) && (b >= 4 && b <= 6)) {
			as.vector(box8)
		} else if ((a >= 7 && a <= 9) && (b >= 7 && b <= 9)) {
			as.vector(box9)
		}
	}

	update.possibles <- function() {
		for (i in 1:81) {
			row.i <- floor((i - 1) / 9) + 1
			col.i <- (i - 1) %% 9 + 1

			possibles[[i]] <<- if (current.grid[row.i, col.i] != 0) {
				vector(mode = "numeric", length = 0)
			} else {
				(1:9)[
					!(1:9 %in%
						unique(c(
							as.vector(current.grid[row.i, ]),
							as.vector(current.grid[, col.i]),
							b2(row.i, col.i)
						)))
				]
			}
		}

		for (i in 1:9) {
			w <- 9 * (i - 1) + 1
			x <- 3 * ((i - 1) %% 3) + 1
			y <- floor((i - 1) / 3) + 1
			z <- x + 27 * (y - 1)
			row.possibles[[i]] <<- c(
				possibles[[w]],
				possibles[[w + 1]],
				possibles[[w + 2]],
				possibles[[w + 3]],
				possibles[[w + 4]],
				possibles[[w + 5]],
				possibles[[w + 6]],
				possibles[[w + 7]],
				possibles[[w + 8]]
			)
			col.possibles[[i]] <<- c(
				possibles[[i]],
				possibles[[i + 9]],
				possibles[[i + 9 * 2]],
				possibles[[i + 9 * 3]],
				possibles[[i + 9 * 4]],
				possibles[[i + 9 * 5]],
				possibles[[i + 9 * 6]],
				possibles[[i + 9 * 7]],
				possibles[[i + 9 * 8]]
			)
			box.possibles[[i]] <<- c(
				possibles[[z]],
				possibles[[z + 1]],
				possibles[[z + 2]],
				possibles[[z + 9]],
				possibles[[z + 9 + 1]],
				possibles[[z + 9 + 2]],
				possibles[[z + 9 * 2]],
				possibles[[z + 9 * 2 + 1]],
				possibles[[z + 9 * 2 + 2]]
			)
		}
	}

	update.grid <- function(cell.number, fill.value) {
		for (j in 1:length(cell.number)) {
			row.j <- floor((cell.number[j] - 1) / 9) + 1
			col.j <- (cell.number[j] - 1) %% 9 + 1
			current.grid[row.j, col.j] <<- fill.value[j]
		}
		update.possibles()
	}

	check.ns <- function() {
		update.possibles()
		for (i in 1:81) {
			if (length(possibles[[i]]) == 1) {
				update.grid(i, possibles[[i]])
			}
		}
	}

	check.hs <- function() {
		update.possibles()
		for (i in 1:9) {
			if (
				sum(
					!(duplicated(row.possibles[[i]]) |
						duplicated(row.possibles[[i]], fromLast = TRUE))
				) >
					0
			) {
				replacement <- row.possibles[[i]][which(
					!(duplicated(row.possibles[[i]]) |
						duplicated(row.possibles[[i]], fromLast = TRUE))
				)]
				replacement.index.row <- vector(
					mode = "numeric",
					length = length(replacement)
				)
				w <- 9 * (i - 1) + 1
				for (j in 1:length(replacement)) {
					replacement.index.row[j] <- 9 *
						(i - 1) +
						which(
							c(
								replacement[j] %in% possibles[[w]],
								replacement[j] %in% possibles[[w + 1]],
								replacement[j] %in% possibles[[w + 2]],
								replacement[j] %in% possibles[[w + 3]],
								replacement[j] %in% possibles[[w + 4]],
								replacement[j] %in% possibles[[w + 5]],
								replacement[j] %in% possibles[[w + 6]],
								replacement[j] %in% possibles[[w + 7]],
								replacement[j] %in% possibles[[w + 8]]
							) ==
								TRUE
						)
				}
				update.grid(replacement.index.row, replacement)
			}
		}

		for (i in 1:9) {
			if (
				sum(
					!(duplicated(col.possibles[[i]]) |
						duplicated(col.possibles[[i]], fromLast = TRUE))
				) >
					0
			) {
				replacement1 <- col.possibles[[i]][which(
					!(duplicated(col.possibles[[i]]) |
						duplicated(col.possibles[[i]], fromLast = TRUE))
				)]
				replacement.index.col <- vector(
					mode = "numeric",
					length = length(replacement1)
				)
				for (j in 1:length(replacement1)) {
					replacement.index.col[j] <- 9 *
						(which(
							c(
								replacement1[j] %in% possibles[[i]],
								replacement1[j] %in% possibles[[i + 9]],
								replacement1[j] %in% possibles[[i + 9 * 2]],
								replacement1[j] %in% possibles[[i + 9 * 3]],
								replacement1[j] %in% possibles[[i + 9 * 4]],
								replacement1[j] %in% possibles[[i + 9 * 5]],
								replacement1[j] %in% possibles[[i + 9 * 6]],
								replacement1[j] %in% possibles[[i + 9 * 7]],
								replacement1[j] %in% possibles[[i + 9 * 8]]
							) ==
								TRUE
						) -
							1) +
						i
				}
				update.grid(replacement.index.col, replacement1)
			}
		}

		for (i in 1:9) {
			if (
				sum(
					!(duplicated(box.possibles[[i]]) |
						duplicated(box.possibles[[i]], fromLast = TRUE))
				) >
					0
			) {
				replacement2 <- box.possibles[[i]][which(
					!(duplicated(box.possibles[[i]]) |
						duplicated(box.possibles[[i]], fromLast = TRUE))
				)]
				replacement.index.box <- vector(
					mode = "numeric",
					length = length(replacement2)
				)
				x <- 3 * ((i - 1) %% 3) + 1
				y <- 3 * floor((i - 1) / 3) + 1
				z <- x + 9 * (y - 1)
				for (j in 1:length(replacement2)) {
					box.index <- which(
						c(
							replacement2[j] %in% possibles[[z]],
							replacement2[j] %in% possibles[[z + 1]],
							replacement2[j] %in% possibles[[z + 2]],
							replacement2[j] %in% possibles[[z + 9]],
							replacement2[j] %in% possibles[[z + 9 + 1]],
							replacement2[j] %in% possibles[[z + 9 + 2]],
							replacement2[j] %in% possibles[[z + 9 * 2]],
							replacement2[j] %in% possibles[[z + 9 * 2 + 1]],
							replacement2[j] %in% possibles[[z + 9 * 2 + 2]]
						) ==
							TRUE
					)
					box.x <- ((box.index - 1) %% 3) + 1
					box.y <- floor((box.index - 1) / 3) + 1
					replacement.index.box[j] <- (x + box.x - 2) +
						(y + box.y - 2) * 9 +
						1
				}
				update.grid(replacement.index.box, replacement2)
			}
		}
	}

	check.np <- function() {
		update.possibles()
	}

	check.hp <- function() {
		update.possibles()
	}

	check.ir <- function() {
		update.possibles()
	}

	check.xw <- function() {
		update.possibles()
	}

	check.yw <- function() {
		update.possibles()
	}

	check.nt <- function() {
		update.possibles()
	}

	check.ht <- function() {
		update.possibles()
	}

	check.nq <- function() {
		update.possibles()
	}

	check.hq <- function() {
		update.possibles()
	}

	check.er <- function() {
		update.possibles()
	}

	solve.grid.easy <- function() {
		for (i in 1:3) {
			check.ns()
			check.hs()
		}
	}
	solve.grid.medium <- function() {
		for (i in 1:3) {
			check.ns()
			check.hs()
			check.np()
			check.ir()
		}
	}
	solve.grid.hard <- function() {
		for (i in 1:3) {
			check.ns()
			check.hs()
			check.np()
			check.ir()
			check.xw()
			check.yw()
		}
	}

	sudoku.grid.solved <<- get.grid()
	current.grid <- sudoku.grid.solved
	sudoku.grid.unsolved <- sudoku.grid.solved
	temporary.grid <- sudoku.grid.solved
	solveable <- TRUE
	while (solveable == TRUE) {
		cell.number1 <- sample(x = 1:81, size = 81, replace = FALSE)
		for (i in cell.number1) {
			row.k <- floor((cell.number1[i] - 1) / 9) + 1
			col.k <- (cell.number1[i] - 1) %% 9 + 1
			sudoku.grid.unsolved <- temporary.grid
			temporary.grid[row.k, col.k] <- 0
			current.grid <- temporary.grid

			if (difficulty == "easy") {
				solve.grid.easy()
			} else if (difficulty == "medium") {
				solve.grid.medium()
			} else if (difficulty == "hard") {
				solve.grid.hard()
			} else {
				solve.grid.easy()
			}

			if (sum(current.grid) == 405) {
				solveable <- TRUE
			} else {
				temporary.grid <- sudoku.grid.unsolved
			}
		}
		solveable <- FALSE
	}
	sudoku.grid.unsolved <<- sudoku.grid.unsolved
}


lets.play.sudoku <- function() {
	display.grid <<- function() {
		grid.color.1 <- c(
			rep(c(rep("white", 9), rep("grey", 9), rep("white", 9)), 9),
			rep(c(rep("grey", 9), rep("white", 9), rep("grey", 9)), 9),
			rep(c(rep("white", 9), rep("grey", 9), rep("white", 9)), 9)
		)

		image(
			1:27,
			1:27,
			matrix(1:(27 * 27), 27, 27, T),
			col = as.vector(grid.color.1),
			axes = F,
			xlab = "",
			ylab = ""
		)
		grid(nx = 9, ny = 9, col = "black", lty = "solid", lwd = 1)
		text(
			((bad.cells$col - 1) * 3 + 2),
			((bad.cells$row - 1) * 3 + 2),
			labels = sudoku.grid.unsolved[bad.cells$cell.number],
			col = "black",
			cex = 2.5
		)
		text(
			3 * floor((which(!(1:81 %in% bad.cells$cell.number)) - 1) / 9) + 2,
			26 - 3 * ((which(!(1:81 %in% bad.cells$cell.number)) - 1) %% 9),
			labels = replace(
				sudoku.grid.unsolved[-bad.cells$cell.number],
				which(sudoku.grid.unsolved[-bad.cells$cell.number] == 0),
				""
			),
			col = "blue",
			cex = 2.5
		)

		text(
			x = sort(rep(1:27, 27)),
			y = rep(27:1, 27),
			labels = replace(note.grid, which(note.grid == 0), ""),
			col = "red",
			cex = .75
		)
	}

	place.value <<- function(row, col, val) {
		if (!(val %in% 0:9)) {
			print("I'm sorry but that is a bad value")
		} else if (length(row) != 1) {
			print("It looks like your row is not length 1")
		} else if (length(col) != 1) {
			print("It looks like your column is not length 1")
		} else if (length(val) != 1) {
			print("It looks like your value is not length 1")
		} else if (!(row %in% 1:9)) {
			print("I'm sorry but that is a bad row value")
		} else if (!(col %in% 1:9)) {
			print("I'm sorry but that is a bad column value")
		} else {
			cell.number <- (col - 1) * 9 + 1 + (row - 1)
			if (cell.number %in% bad.cells$cell.number) {
				print("I'm sorry but you can't place a number there. Try again")
			} else {
				list.of.moves[[length(list.of.moves) + 1]] <<- vector(
					mode = "list",
					length = 0
				)
				list.of.moves[[length(list.of.moves)]][[1]] <<- c(
					val,
					row,
					col,
					sudoku.grid.unsolved[row, col],
					0
				)
				list.of.moves[[length(list.of.moves)]][[2]] <<- note.grid[
					3 * (row - 1) + (0:2) + 1,
					3 * (col - 1) + (0:2) + 1
				]
				list.of.moves[[length(list.of.moves)]][[3]] <<- note.grid[
					3 * (0:8) + floor((val - 1) / 3) + 1,
					3 * (col - 1) + (val - 1) %% 3 + 1
				]
				list.of.moves[[length(list.of.moves)]][[4]] <<- note.grid[
					3 * (row - 1) + floor((val - 1) / 3) + 1,
					3 * (0:8) + (val - 1) %% 3 + 1
				]
				list.of.moves[[length(list.of.moves)]][[5]] <<- note.grid[
					3 *
						(3 * floor((row - 1) / 3)):(3 *
							floor((row - 1) / 3) +
							2) +
						floor((val - 1) / 3) +
						1,
					3 *
						(3 * floor((col - 1) / 3)):(3 *
							floor((col - 1) / 3) +
							2) +
						(val - 1) %% 3 +
						1
				]
				sudoku.grid.unsolved[row, col] <<- val

				if (val > 0) {
					note.grid[
						3 * (row - 1) + (0:2) + 1,
						3 * (col - 1) + (0:2) + 1
					] <<- 0
					note.grid[
						3 * (0:8) + floor((val - 1) / 3) + 1,
						3 * (col - 1) + (val - 1) %% 3 + 1
					] <<- 0
					note.grid[
						3 * (row - 1) + floor((val - 1) / 3) + 1,
						3 * (0:8) + (val - 1) %% 3 + 1
					] <<- 0
					note.grid[
						3 *
							(3 * floor((row - 1) / 3)):(3 *
								floor((row - 1) / 3) +
								2) +
							floor((val - 1) / 3) +
							1,
						3 *
							(3 * floor((col - 1) / 3)):(3 *
								floor((col - 1) / 3) +
								2) +
							(val - 1) %% 3 +
							1
					] <<- 0
				}
			}
		}

		display.grid()

		if (!(0 %in% sudoku.grid.unsolved)) {
			check.grid()
		}
	}

	place.note <<- function(row, col, val) {
		if (!(val %in% 1:9)) {
			print("I'm sorry but that is a bad value")
		} else if (length(row) != 1) {
			print("It looks like your row is not length 1")
		} else if (length(col) != 1) {
			print("It looks like your column is not length 1")
		} else if (length(val) != 1) {
			print("It looks like your value is not length 1")
		} else if (!(row %in% 1:9)) {
			print("I'm sorry but that is a bad row value")
		} else if (!(col %in% 1:9)) {
			print("I'm sorry but that is a bad column value")
		} else {
			cell.number <- (col - 1) * 9 + 1 + (row - 1)
			if (cell.number %in% bad.cells$cell.number) {
				print("I'm sorry but you can't place a note there. Try again")
			} else {
				list.of.moves[[length(list.of.moves) + 1]] <<- vector(
					mode = "list",
					length = 0
				)
				list.of.moves[[length(list.of.moves)]][[1]] <<- c(
					val,
					row,
					col,
					note.grid[
						3 * (row - 1) + floor((val - 1) / 3) + 1,
						3 * (col - 1) + (val - 1) %% 3 + 1
					],
					1,
					0
				)
				note.grid[
					3 * (row - 1) + floor((val - 1) / 3) + 1,
					3 * (col - 1) + (val - 1) %% 3 + 1
				] <<- val
			}
		}
		display.grid()
	}

	remove.note <<- function(row, col, val) {
		if (!(val %in% 1:9)) {
			print("I'm sorry but that is a bad value")
		} else if (!(row %in% 1:9)) {
			print("I'm sorry but that is a bad row value")
		} else if (!(col %in% 1:9)) {
			print("I'm sorry but that is a bad column value")
		} else {
			cell.number <- (col - 1) * 9 + 1 + (row - 1)
			list.of.moves[[length(list.of.moves) + 1]] <<- vector(
				mode = "list",
				length = 0
			)
			list.of.moves[[length(list.of.moves)]][[1]] <<- c(
				val,
				row,
				col,
				note.grid[
					3 * (row - 1) + floor((val - 1) / 3) + 1,
					3 * (col - 1) + (val - 1) %% 3 + 1
				],
				1,
				1
			)
			note.grid[
				3 * (row - 1) + floor((val - 1) / 3) + 1,
				3 * (col - 1) + (val - 1) %% 3 + 1
			] <<- 0
		}
		display.grid()

		if (!(0 %in% sudoku.grid.unsolved)) {
			check.grid()
		}
	}

	reset.grid <<- function() {
		sudoku.grid.unsolved <<- sudoku.grid.solved
		sudoku.grid.unsolved[-bad.cells$cell.number] <<- 0
		note.grid <<- matrix(
			data = vector(mode = "integer", length = 27 * 27),
			ncol = 27,
			nrow = 27,
			byrow = T
		)
		display.grid()
	}

	undo.move <<- function() {
		if (length(list.of.moves) > 1) {
			if (list.of.moves[[length(list.of.moves)]][[1]][5] == 0) {
				sudoku.grid.unsolved[
					list.of.moves[[length(list.of.moves)]][[1]][2],
					list.of.moves[[length(list.of.moves)]][[1]][3]
				] <<- list.of.moves[[length(list.of.moves)]][[1]][4]
				note.grid[
					3 *
						(list.of.moves[[length(list.of.moves)]][[1]][2] - 1) +
						(0:2) +
						1,
					3 *
						(list.of.moves[[length(list.of.moves)]][[1]][3] - 1) +
						(0:2) +
						1
				] <<- list.of.moves[[length(list.of.moves)]][[2]]
				note.grid[
					3 *
						(0:8) +
						floor(
							(list.of.moves[[length(list.of.moves)]][[1]][1] -
								1) /
								3
						) +
						1,
					3 *
						(list.of.moves[[length(list.of.moves)]][[1]][3] - 1) +
						(list.of.moves[[length(list.of.moves)]][[1]][1] - 1) %%
							3 +
						1
				] <<- list.of.moves[[length(list.of.moves)]][[3]]
				note.grid[
					3 *
						(list.of.moves[[length(list.of.moves)]][[1]][2] - 1) +
						floor(
							(list.of.moves[[length(list.of.moves)]][[1]][1] -
								1) /
								3
						) +
						1,
					3 *
						(0:8) +
						(list.of.moves[[length(list.of.moves)]][[1]][1] - 1) %%
							3 +
						1
				] <<- list.of.moves[[length(list.of.moves)]][[4]]
				note.grid[
					3 *
						(3 *
							floor(
								(list.of.moves[[length(list.of.moves)]][[1]][
									2
								] -
									1) /
									3
							)):(3 *
							floor(
								(list.of.moves[[length(list.of.moves)]][[1]][
									2
								] -
									1) /
									3
							) +
							2) +
						floor(
							(list.of.moves[[length(list.of.moves)]][[1]][1] -
								1) /
								3
						) +
						1,
					3 *
						(3 *
							floor(
								(list.of.moves[[length(list.of.moves)]][[1]][
									3
								] -
									1) /
									3
							)):(3 *
							floor(
								(list.of.moves[[length(list.of.moves)]][[1]][
									3
								] -
									1) /
									3
							) +
							2) +
						(list.of.moves[[length(list.of.moves)]][[1]][1] - 1) %%
							3 +
						1
				] <<- list.of.moves[[length(list.of.moves)]][[5]]
				display.grid()
			} else if (list.of.moves[[length(list.of.moves)]][[1]][5] == 1) {
				if (list.of.moves[[length(list.of.moves)]][[1]][6] == 0) {
					note.grid[
						3 *
							(list.of.moves[[length(list.of.moves)]][[1]][2] -
								1) +
							floor(
								(list.of.moves[[length(list.of.moves)]][[1]][
									1
								] -
									1) /
									3
							) +
							1,
						3 *
							(list.of.moves[[length(list.of.moves)]][[1]][3] -
								1) +
							(list.of.moves[[length(list.of.moves)]][[1]][1] -
								1) %%
								3 +
							1
					] <<- list.of.moves[[length(list.of.moves)]][[1]][4]
					display.grid()
				} else if (list.of.moves[[length(list.of.moves)]][[1]][6] == 1) {
					note.grid[
						3 *
							(list.of.moves[[length(list.of.moves)]][[1]][2] -
								1) +
							floor(
								(list.of.moves[[length(list.of.moves)]][[1]][
									1
								] -
									1) /
									3
							) +
							1,
						3 *
							(list.of.moves[[length(list.of.moves)]][[1]][3] -
								1) +
							(list.of.moves[[length(list.of.moves)]][[1]][1] -
								1) %%
								3 +
							1
					] <<- list.of.moves[[length(list.of.moves)]][[1]][4]
					display.grid()
				}
			}
		} else if (length(list.of.moves) == 1) {
			if (list.of.moves[[length(list.of.moves)]][[1]][5] == 0) {
				sudoku.grid.unsolved[
					list.of.moves[[length(list.of.moves)]][[1]][2],
					list.of.moves[[length(list.of.moves)]][[1]][3]
				] <<- 0
				display.grid()
			} else if (list.of.moves[[length(list.of.moves)]][[1]][5] == 1) {
				note.grid[
					3 *
						(list.of.moves[[length(list.of.moves)]][[1]][2] - 1) +
						floor(
							(list.of.moves[[length(list.of.moves)]][[1]][1] -
								1) /
								3
						) +
						1,
					3 *
						(list.of.moves[[length(list.of.moves)]][[1]][3] - 1) +
						(list.of.moves[[length(list.of.moves)]][[1]][1] - 1) %%
							3 +
						1
				] <<- 0
				display.grid()
			}
		} else {
			print("Nothing to undo")
		}
		list.of.moves[length(list.of.moves)] <<- NULL
	}

	check.grid <<- function() {
		if (sum(sudoku.grid.unsolved == sudoku.grid.solved) == 81) {
			print(
				"Congratulations you've solved it! OMG you are the bestest at this game :) use the \'get.new.grid\' function to play a different puzzle."
			)
		} else if (
			sum(
				!(sudoku.grid.unsolved[which(sudoku.grid.unsolved != 0)] ==
					sudoku.grid.solved[which(sudoku.grid.unsolved != 0)])
			) ==
				0
		) {
			print("So far so good. I dont see any mistakes yet. Keep going!")
		} else if (
			sum(
				!(sudoku.grid.unsolved[which(sudoku.grid.unsolved != 0)] ==
					sudoku.grid.solved[which(sudoku.grid.unsolved != 0)])
			) >
				0
		) {
			print(
				"I\'m sorry but you\'ve made a mistake somewhere. Use the \'mistakes\' function to see where."
			)
			print(
				"*Note: this function does not exist yet. I will update this once I code that functionality*"
			)
		}
	}

	get.new.grid <<- function() {
		set.grid()

		list.of.moves <<- vector(mode = "list", length = 0)
		bad.cells <<- vector(mode = "list", length = 0)
		bad.cells$cell.number <<- which(sudoku.grid.unsolved != 0)
		bad.cells$col <<- floor((bad.cells$cell.number - 1) / 9) + 1
		bad.cells$row <<- 9 - (bad.cells$cell.number - 1) %% 9

		note.grid <<- matrix(
			data = vector(mode = "integer", length = 27 * 27),
			ncol = 27,
			nrow = 27,
			byrow = TRUE
		)

		display.grid()

		readline(
			"Use the \'place.value\' function to put a number in the grid.\nYou need to tell it the row, column, and the number you want to place.\nPress \'Enter\' to continue"
		)
		readline(
			"Use the \'undo.move\' function to undo your last move.\nPress \'Enter\' to continue"
		)
		readline(
			"Use the \'reset.grid\' function to reset the grid to the beginning.\nBe care as you will lose all progress.\nPress \'Enter\' to continue "
		)
		readline(
			"Use the \'check.grid\' function to check and see if you've solved it.\nPress \'Enter\' to continue"
		)
		readline(
			"Use the \'get.new.grid\' function to start a brand new puzzle.\nPress \'Enter\' to continue and start the puzzle"
		)
	}

	command1 <- readline(
		prompt = "Hello! Are you ready to play sudoku?\nType \'rules\' if you want to look over the rules first or\n\'play\' if you already know them and are ready to start: "
	)
	how.to.play <- "The rules of sudoku are simple: you are to place the numbers 1-9 in every row, column, and 3x3 box without repeating."

	while (
		(command1 != "play") &&
			(command1 != "rules") &&
			(command1 != "help") &&
			(command1 != "exit")
	) {
		command1 <- readline(
			prompt = "please type \'rules\', \'play\', \'help\' or \'exit\': "
		)
	}

	if (command1 == "play") {
		get.new.grid()
	} else if (command1 == "rules") {
		command2 <- "no"
		while (command2 == "no") {
			print(how.to.play)
			command2 <- readline(prompt = "Are you ready to play now?: ")
			while ((command2 != "yes") && (command2 != "no")) {
				command2 <- readline(prompt = "please type \'yes\' or \'no\': ")
			}
		}
		get.new.grid()
	} else if (command1 == "help") {
		print("what on earth do you need help with??? Just type \'play\'")
		readline(prompt = "")
		get.new.grid()
	} else if (command1 == "exit") {
		print("Sorry to see you go. Please come back and play again sometime!")
	}
}
