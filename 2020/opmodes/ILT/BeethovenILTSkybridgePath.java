package opmodes.ILT;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/5/2019.
 */
public class BeethovenILTSkybridgePath {
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

    public BeethovenILTSkybridgePath() {
        // BLUE ALLIANCE, BUILDING SIDE
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 26, -0.8); // move to foundation
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); // pull foundation back
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 120, -0.5); // turn the foundation into the building
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, -0.8);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 16, -0.4);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 42, 0.8);

        // BLUE ALLIANCE, LOADING SIDE
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, -0.5);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[BeethovenILTSkybridgePath.AllianceColor.BLUE.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);

        // RED ALLIANCE, BUILDING SIDE
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 26, -0.8); // move to foundation
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.2); // touch foundation
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.5); // pull foundation back
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 120, 0.7); // turn the foundation into the building
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, -0.8); // push foundation against wall
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addPause(1000); // phantom segment for arms
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 16, 0.4);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 35, 0.8);

        // RED ALLIANCE, LOADING SIDE
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.8);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 10, 0.2);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 72, 0.5);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 60, 0.8);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addPause(1000);
        paths[BeethovenILTSkybridgePath.AllianceColor.RED.ordinal()][BeethovenILTSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 20, -0.8);
    }

    public DeadReckonPath getPath(BeethovenILTSkybridgePath.AllianceColor allianceColor, BeethovenILTSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
