markdown# Dice Roller MCP Server

A Model Context Protocol (MCP) server that provides comprehensive dice rolling functionality for tabletop games, RPGs, and random number generation.

## Purpose

This MCP server provides a secure interface for AI assistants to perform various dice rolling operations commonly used in tabletop role-playing games like Dungeons & Dragons, board games, and general randomization tasks.

## Features

### Current Implementation
- **`flip_coin`** - Flip one or more coins and display results as heads or tails
- **`roll_dice`** - Roll dice using standard notation (e.g., 2d6+3, 1d20-2)
- **`roll_custom`** - Roll custom dice with any number of sides
- **`roll_stats`** - Generate D&D ability scores using 4d6 drop lowest method
- **`roll_advantage`** - Roll d20 with advantage (roll twice, take higher)
- **`roll_disadvantage`** - Roll d20 with disadvantage (roll twice, take lower)
- **`roll_check`** - Make a skill check against a difficulty class (DC)
- **`roll_initiative`** - Roll initiative for combat encounters

## Prerequisites

- Docker Desktop with MCP Toolkit enabled
- Docker MCP CLI plugin (`docker mcp` command)

## Installation

See the step-by-step instructions provided with the files.

## Usage Examples

In Claude Desktop, you can ask:
- "Flip a coin for me"
- "Roll 2d6+3 for damage"
- "Generate D&D stats for a new character"
- "Roll a perception check with +5 modifier against DC 15"
- "Roll initiative for 4 combatants with +2 dex bonus"
- "Roll with advantage and add my +7 attack bonus"
- "Roll a d100 for me"
- "Roll 3d8 for healing"
- "Make a stealth check with disadvantage"

### Dice Notation Guide
- **XdY**: Roll X dice with Y sides (e.g., 3d6 = roll three six-sided dice)
- **XdY+Z**: Add Z to the total (e.g., 1d20+5)
- **XdY-Z**: Subtract Z from the total (e.g., 2d6-2)
- **d20**: When X is omitted, defaults to 1 die

### Common D&D Dice
- **d4**: Four-sided die (1-4)
- **d6**: Six-sided die (1-6)
- **d8**: Eight-sided die (1-8)
- **d10**: Ten-sided die (1-10)
- **d12**: Twelve-sided die (1-12)
- **d20**: Twenty-sided die (1-20)
- **d100**: Percentile die (1-100)

## Architecture
Claude Desktop → MCP Gateway → Dice MCP Server
↓
Random Generation
Engine

## Development

### Local Testing
```bash
# Run directly
python dice_server.py

# Test MCP protocol
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | python dice_server.py
Adding New Tools

Add the function to dice_server.py
Decorate with @mcp.tool()
Update the catalog entry with the new tool name
Rebuild the Docker image

Troubleshooting
Tools Not Appearing

Verify Docker image built successfully
Check catalog and registry files
Ensure Claude Desktop config includes custom catalog
Restart Claude Desktop

Unexpected Results

Verify parameters are in correct format
Check dice notation is valid
Ensure modifiers are integers

Security Considerations

Running as non-root user
No external API calls or data storage
All randomization uses Python's standard random module

License
MIT License

## File 5: CLAUDE.md
```markdown
# CLAUDE.md - Dice Roller MCP Server

## Overview
This MCP server provides dice rolling functionality for tabletop gaming and randomization. It supports standard dice notation, D&D-specific mechanics, and general random number generation.

## Implementation Details

### Core Design Principles
- **Simplicity**: Clean, straightforward dice rolling without complexity
- **Gaming Focus**: Optimized for tabletop RPG mechanics
- **Standard Notation**: Supports common dice notation (XdY+Z)
- **Error Handling**: Graceful handling of invalid inputs
- **Visual Feedback**: Uses emojis and formatting for clear results

### Tool Categories

#### Basic Rolling
- `flip_coin`: Simple binary randomization
- `roll_dice`: Standard notation support
- `roll_custom`: Flexible dice configuration

#### D&D Specific
- `roll_stats`: Character generation
- `roll_advantage`: 5e advantage mechanic
- `roll_disadvantage`: 5e disadvantage mechanic
- `roll_check`: Skill checks against DC
- `roll_initiative`: Combat order determination

### Technical Implementation

#### Random Number Generation
- Uses Python's `random` module
- Uniform distribution for fair results
- No seed setting for true randomness

#### Input Validation
- Accepts string parameters with defaults
- Validates ranges (1-100 dice, 2-1000 sides)
- Handles empty inputs gracefully

#### Output Formatting
- Clear visual hierarchy with emojis
- Bold for important results
- Detailed breakdowns for complex rolls
- Special notifications for criticals

### Usage Patterns

#### Simple Requests
"Roll a d20" → roll_dice("1d20")
"Flip 3 coins" → flip_coin("3")

#### Complex Gaming Scenarios
"Attack with my +7 sword" → roll_dice("1d20+7")
"Roll for fireball damage" → roll_dice("8d6")
"Check for traps, +3 perception" → roll_check("15", "3", "Perception")

#### Character Creation
"Generate stats for a fighter" → roll_stats()
"Roll hit points for level 3" → roll_dice("3d10")

### Error Handling Strategy
1. **Invalid notation**: Return helpful error message
2. **Out of range**: Specify valid ranges
3. **Type errors**: Convert when possible, error when not
4. **Empty inputs**: Use sensible defaults

### Future Enhancement Ideas
- Exploding dice mechanics
- Dice pool systems (count successes)
- Savage Worlds mechanics (exploding, wild die)
- Fate/Fudge dice
- Custom random tables
- Roll history tracking
- Statistical analysis of rolls

### Testing Guidelines

#### Basic Tests
- Roll single die: `roll_dice("1d20")`
- Roll with modifier: `roll_dice("2d6+3")`
- Negative modifier: `roll_dice("1d20-2")`

#### Edge Cases
- Maximum dice: `roll_dice("100d6")`
- Minimum sides: `roll_custom("2", "1")`
- Invalid input: `roll_dice("invalid")`
- Empty input: `roll_dice("")`

#### D&D Scenarios
- Attack roll: `roll_advantage("5")`
- Saving throw: `roll_check("14", "2", "Constitution Save")`
- Damage roll: `roll_dice("2d6+4")`
- Initiative: `roll_initiative("2", "5")`

### Performance Considerations
- Lightweight operations (no external calls)
- Instant response times
- Memory efficient (no storage)
- Stateless design

### User Experience Notes
- Natural language friendly
- Supports common gaming terminology
- Clear success/failure indicators
- Celebrates critical rolls
- Educational dice notation examples

## Best Practices for Extension

When adding new dice mechanics:
1. Keep functions focused and single-purpose
2. Use consistent parameter naming
3. Provide helpful defaults
4. Format output consistently
5. Include relevant emojis for visual clarity
6. Validate all inputs thoroughly
7. Return clear error messages
8. Test with common use cases

## Common Issues and Solutions

**Issue**: Dice notation not recognized
**Solution**: Ensure format matches XdY±Z pattern

**Issue**: Results seem non-random
**Solution**: Python's random is pseudorandom but sufficient for gaming

**Issue**: Need more complex mechanics
**Solution**: Combine multiple tool calls or extend functionality

## Integration Tips

- Can be combined with other MCP servers for game management
- Results can feed into narrative generation
- Useful for probability demonstrations
- Can simulate various game systems

## References
- D&D 5e Basic Rules
- Standard dice notation conventions
- Common tabletop RPG mechanics
- Probability in gaming