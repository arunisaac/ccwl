cwlVersion: v1.2
class: CommandLineTool
baseCommand: echo
inputs:
  - id: spam
    type: string
    inputBinding:
      position: 1
outputs:
  ham:
    type: stdout
  eggs: stdout
