package opmodes.Reggie;

import com.qualcomm.hardware.bosch.BNO055IMU;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import opmodes.Utilities.VivaldiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.IMUGyroTask;
import team25core.MineralDetectionTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RunToEncoderValueTask;
import team25core.SingleShotTimerTask;

/**
 * Created by Lizzie on 11/27/2018.
 */
@Autonomous(name = "Reggie Master Autonomous")
public class ReggieLisztAutonomous extends Robot {

    // declaring motors, servos, and drivetrain
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private FourWheelDirectDrivetrain drivetrain;
    private Servo marker;

    // lift variables
    private DcMotor liftLeft;
    private DcMotor liftRight;
    private boolean leftLanded = false;
    private boolean rightLanded = false;
    BNO055IMU imu;
    private Telemetry.Item gyroItem;
    private IMUGyroTask gyroTask;
    private DeadReckonPath liftBufferPath;
    private DeadReckonTask liftBufferTask;


    // knock paths and utilities
    private DeadReckonPath knockPath;
    private ReggieDropoffUtil dropoff;
    private DeadReckonPath exitDepotPath;
    private ReggieExitDepotDropoff exitDepotDropoff;

    // declaring gamepad variables
    private GamepadTask gamepad1;
    private GamepadTask gamepad2;
    public static ReggieDropoffUtil.MineralPosition goldMineralPosition;
    public static ReggieDropoffUtil.StartingPosition robotStartingPosition;
    public static ReggieDropoffUtil.EndingPosition robotEndingPosition;
    public static ReggieDropoffUtil.HangingPosition robotHangingPosition;


    // declaring telemetry item
    private Telemetry.Item numberOfMineralsItem;
    private Telemetry.Item goldMineralPositionItem;
    private Telemetry.Item startingPositionItem;
    private Telemetry.Item hangingItem;
    private Telemetry.Item endingPositionItem;

    @Override
    public void init() {
        // drivetrain initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        // lift initialization
        liftLeft = hardwareMap.dcMotor.get("liftLeft");
        liftRight = hardwareMap.dcMotor.get("liftRight");
        liftLeft.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftLeft.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftRight.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        liftRight.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        liftBufferPath = new DeadReckonPath();
        liftBufferPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 1, 0.3);
        liftBufferTask = new DeadReckonTask(this, liftBufferPath, drivetrain);

        /*
        // bungee box initialization
        bungeeBox = hardwareMap.dcMotor.get("bungeeBox");
        bungeeBox.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        bungeeBox.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
        // bungeeBox.setZeroPowerBehavior(DcMotor.ZeroPowerBehavior.BRAKE);
        */

        // depot path
        knockPath = new DeadReckonPath();
        dropoff = new ReggieDropoffUtil();
        exitDepotPath = new DeadReckonPath();
        exitDepotDropoff = new ReggieExitDepotDropoff();

        // initializing gamepad variables
        gamepad1 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad1);
        gamepad2 = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2);
        addTask(gamepad2);

        // initializing telemetry items
        numberOfMineralsItem = telemetry.addData("Number of Minerals: ", "NOT DETECTED");
        goldMineralPositionItem = telemetry.addData("Gold Mineral Position: ", "NOT SELECTED");
        startingPositionItem = telemetry.addData("Starting Position", "NOT SELECTED");
        hangingItem = telemetry.addData("Hanging: ", "NOT SELECTED");
        endingPositionItem = telemetry.addData("Ending Position: ", "NOT SELECTED");

        // initializing mineral detection, marker movement, and IMU
        initializeMineralDetection();
        imu = hardwareMap.get(BNO055IMU.class, "IMU");
        gyroItem = telemetry.addData("Gyro state:", "Not at target");
        marker = hardwareMap.servo.get("marker");
        handleGyroEvent();
    }

    protected void initializeMineralDetection() {
        String cameraName = "mineralCamera";
        MineralDetectionTask mdTask = new MineralDetectionTask(this, cameraName) {
            public void handleEvent(RobotEvent e) {
                MineralDetectionEvent event = (MineralDetectionEvent) e;
                List<Recognition> updatedMinerals = event.minerals;
                numberOfMineralsItem.setValue(updatedMinerals.size());
                ReggieDropoffUtil.MineralPosition goldPos = ReggieDropoffUtil.determineGoldPosition(updatedMinerals);
                ReggieDropoffUtil.sendPositionTelemetry(goldPos, goldMineralPositionItem);

                switch (goldPos) {
                    case LEFT:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.LEFT;
                        break;
                    case RIGHT:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.RIGHT;
                        break;
                    case CENTER:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.CENTER;
                        break;
                    case UNKNOWN:
                        goldMineralPosition = ReggieDropoffUtil.MineralPosition.CENTER;
                        break;
                }
            }
        };
        mdTask.init(telemetry, hardwareMap);
        mdTask.setDetectionKind(MineralDetectionTask.DetectionKind.EVERYTHING);
        this.addTask(mdTask);
    }



    @Override
    public void start() {
        if(robotHangingPosition == ReggieDropoffUtil.HangingPosition.HANGING) {
            unlatch();
            knockPath = dropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
            exitDepotPath = exitDepotDropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
        } else if (robotHangingPosition == ReggieDropoffUtil.HangingPosition.NOT_HANGING) {
            knockPath = dropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
            exitDepotPath = exitDepotDropoff.getPath(robotStartingPosition, robotEndingPosition, goldMineralPosition);
            initialMove(knockPath);
        }
    }

    @Override
    public void handleEvent(RobotEvent e) {
        if (e instanceof GamepadTask.GamepadEvent) {
            // starting position, hanging, parking, gold mineral position, double sampling
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            switch (event.kind) {
                case BUTTON_X_DOWN:
                    endingPositionItem.setValue("PARKING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.PARKING;
                    break;
                case BUTTON_Y_DOWN:
                    endingPositionItem.setValue("NOT PARKING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.NOT_PARKING;
                    break;
                case BUTTON_A_DOWN:
                    endingPositionItem.setValue("DOUBLE SAMPLING");
                    robotEndingPosition = ReggieDropoffUtil.EndingPosition.DOUBLE_SAMPLING;
                    break;
                case RIGHT_BUMPER_DOWN:
                    startingPositionItem.setValue("DEPOT");
                    robotStartingPosition = ReggieDropoffUtil.StartingPosition.DEPOT;
                    break;
                case RIGHT_TRIGGER_DOWN:
                    startingPositionItem.setValue("CRATER");
                    robotStartingPosition = ReggieDropoffUtil.StartingPosition.CRATER;
                    break;
                case LEFT_BUMPER_DOWN:
                    hangingItem.setValue("HANGING");
                    robotHangingPosition = ReggieDropoffUtil.HangingPosition.HANGING;
                    break;
                case LEFT_TRIGGER_DOWN:
                    hangingItem.setValue("NOT HANGING");
                    robotHangingPosition = ReggieDropoffUtil.HangingPosition.NOT_HANGING;
                    break;
            }
        }
    }

    public void handleGyroEvent ()
    {
        gyroTask = new IMUGyroTask(this, imu, 0, true) {
            @Override
            public void handleEvent (RobotEvent event) {
                if(((IMUGyroEvent) event).kind == EventKind.HIT_TARGET) {
                    drivetrain.stop();
                    bufferMove (liftBufferPath);
                } else if (((IMUGyroEvent) event).kind == EventKind.PAST_TARGET) {
                    drivetrain.turn(VivaldiCalibration.TURN_SPEED / 2);
                }
            }
        };
        gyroTask.init();
    }

    public void bufferMove (final DeadReckonPath path)
    {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        initialMove(knockPath);
                        break;
                }
            }
        });
    }


    // add the prompt to initial move after unlatch
    public void unlatch()
    {
        RobotLog.i("251 - Going UP");
        RunToEncoderValueTask leftTask = new RunToEncoderValueTask(this, liftLeft, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_LEFT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE L");
                    liftLeft.setPower(0.0);
                    leftLanded = true;
                    if (rightLanded = true) {
                        initialStraighten();
                    }
                }
            }
        };
        RunToEncoderValueTask rightTask = new RunToEncoderValueTask(this, liftRight, VivaldiCalibration.LIFT_ENCODER_COUNT, VivaldiCalibration.LIFT_RIGHT_UP) {
            @Override
            public void handleEvent(RobotEvent event) {
                if (((RunToEncoderValueEvent) event).kind == EventKind.DONE) {
                    RobotLog.i("251 - DONE R");
                    liftRight.setPower(0.0);
                    rightLanded = true;
                    if (leftLanded == true) {
                        initialStraighten();
                    }
                }
            }
        };
        addTask(leftTask);
        addTask(rightTask);
    }

    public void initialStraighten()
    {
        addTask(gyroTask);
        drivetrain.turn(-VivaldiCalibration.TURN_SPEED);
    }

    protected void initialMove(final DeadReckonPath path) {
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        if (robotStartingPosition == ReggieDropoffUtil.StartingPosition.DEPOT) {
                            markerDrop();
                        } else {
                            exitDepot(exitDepotPath);
                        }
                        break;
                }
            }
        });
    }

    protected void markerDrop() {
        RobotLog.i("Carrying out marker drop method.");
        addTask(new SingleShotTimerTask(this, 600) {
            @Override
            public void handleEvent(RobotEvent e) {
                marker.setPosition(VivaldiCalibration.MARKER_DEPLOYED);
                exitDepot(exitDepotPath);
            }
        });
    }

    protected void exitDepot(final DeadReckonPath path) {
        RobotLog.i("Carrying out exit depot method.");
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent (RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.i("251: You're done, congrats!");
                }
            }
        });
    }

}
