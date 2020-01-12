package opmodes.LM3;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/5/2019.
 */
public class LisztSkybridgePath {
    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    public enum StartingPosition {
        BUILDING,
        LOADING,
        DEFAULT;
    }

    public enum ArmLocation {
        ARM_STOWED,
        ARM_DEPLOYED,
    }

    DeadReckonPath[][] paths = new DeadReckonPath[2][2];

    public LisztSkybridgePath() {
        // BLUE ALLIANCE, BUILDING SIDE
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 28, -0.8); // move to foundation
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); // pull foundation back
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 120, -0.5); // turn the foundation into the building
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, -0.8);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 14, -0.4);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 46, 0.8);

        // BLUE ALLIANCE, LOADING SIDE
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, -0.5);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[LisztSkybridgePath.AllianceColor.BLUE.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);

        // RED ALLIANCE, BUILDING SIDE
        // doublecheck for phantom segment
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 28, -0.8); // move to foundation
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); // pull foundation back
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 130, 0.5); // turn the foundation into the building
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 15, -0.8); // pull foundation back
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, 0.4); // pull foundation back
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 43, 0.8); // pull foundation back

        // RED ALLIANCE, LOADING SIDE
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, 0.5);
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[LisztSkybridgePath.AllianceColor.RED.ordinal()][LisztSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);
    }

    public DeadReckonPath getPath(LisztSkybridgePath.AllianceColor allianceColor, LisztSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
