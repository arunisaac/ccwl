cwlVersion: v1.2
class: CommandLineTool
baseCommand: echo
arguments: ["-n"]
inputs:
  message:
    type: string
    inputBinding:
      position: 1
outputs:
  output:
    type: stdout
