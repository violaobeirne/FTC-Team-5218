package opmodes.LM0;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.Paths.MarkerDropoff;
import opmodes.Utilities.VivaldiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 10/26/2018.
 */
@Autonomous(name = "League Meet 0 Autonomous")
public class LeagueMeet0MozartAutonomous extends Robot {
    private enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo bungeeBox;
    private DcMotor bbExtension;
    private Servo marker;
    private FourWheelDirectDrivetrain drivetrain;
    private DeadReckonPath exitDepotPath;

    private GamepadTask gamepad;
    protected AllianceColor allianceColor;
    protected MarkerDropoff.StartingPosition startPosition;
    protected MarkerDropoff.DepotPath depotPath;

    private DeadReckonPath markerPath;
    private MarkerDropoff dropoff;

    private Telemetry.Item alliance;
    private Telemetry.Item startingPosition;
    private Telemetry.Item path;

    @Override
    public void init() {
        allianceColor = allianceColor.DEFAULT;
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);


        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        startingPosition = telemetry.addData("Starting Position: ", "NOT SELECTED");
        path = telemetry.addData("Depot Path:", "NOT SELECTED");

        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");

        bungeeBox = hardwareMap.servo.get("bungeeBox");
        bbExtension = hardwareMap.dcMotor.get("bbExtension");
        marker = hardwareMap.servo.get("marker");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        dropoff = new MarkerDropoff();
        // bungeeBox.setPosition(VivaldiCalibration.BUNGEE_BOX_18);
        marker.setPosition(VivaldiCalibration.MARKER_STOWED);

        exitDepotPath = new DeadReckonPath();
        exitDepotPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, -0.2);
    }

    @Override
    public void start() {
        markerPath = dropoff.getPath(startPosition, depotPath);
        initialMove(markerPath);
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamepadSelection(event);
        }
    }

    public void handleGamepadSelection(GamepadTask.GamepadEvent event) {
        switch (event.kind) {
            case BUTTON_X_DOWN:
                allianceColor = AllianceColor.BLUE;
                alliance.setValue("BLUE");
                break;
            case BUTTON_B_DOWN:
                allianceColor = AllianceColor.RED;
                alliance.setValue("RED");
                break;
            case BUTTON_A_DOWN:
                startPosition = MarkerDropoff.StartingPosition.CLOSE;
                startingPosition.setValue("CLOSE");
                break;
            case BUTTON_Y_DOWN:
                startPosition = MarkerDropoff.StartingPosition.FAR;
                startingPosition.setValue("FAR");
                break;
            case LEFT_BUMPER_DOWN:
                depotPath = MarkerDropoff.DepotPath.LEFT;
                path.setValue("LEFT");
                break;
            case RIGHT_BUMPER_DOWN:
                depotPath = MarkerDropoff.DepotPath.RIGHT;
                path.setValue("RIGHT");
                break;
        }
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                    dropMarker();
                }
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:

                }
            }
        });
    }

    protected void dropMarker() {
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                marker.setPosition(VivaldiCalibration.MARKER_DEPLOYED);
                exitDepot(exitDepotPath);
            }
        });
    }


}
